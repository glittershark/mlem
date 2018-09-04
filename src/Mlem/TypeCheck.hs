{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
--------------------------------------------------------------------------------
module Mlem.TypeCheck
  ( runTc
  , tcExpr
  , tcTyp
  ) where
--------------------------------------------------------------------------------
import Mlem.Prelude
import Control.Lens hiding ((:<))
import Control.Monad.State hiding (return)
import Control.Monad.Except hiding (return)
import Data.List.NonEmpty (NonEmpty(..))
--------------------------------------------------------------------------------
import Mlem.AST
import Mlem.Util
import Mlem.Util.ShortCircuit
import Mlem.Data.Vec (Vec(..))
import qualified Mlem.Data.Vec as Vec
--------------------------------------------------------------------------------

data TcState where
  TcState
    :: { _tyVars    :: HashMap TyVar Typ
      , _vars      :: HashMap Name Typ
      , _tvCounter :: Word32
      }
    -> TcState
makeLenses ''TcState

instance Semigroup TcState where
  TcState tv1 v1 c1 <> TcState tv2 v2 c2
    = TcState (tv1 <> tv2) (v1 <> v2) (max c1 c2)

instance Monoid TcState where
  mempty = TcState mempty mempty 0

--------------------------------------------------------------------------------

newtype TcError = TcError String
  deriving stock   (Show, Eq)
  deriving newtype (IsString)

newtype TcM a = TcM {
    unTcM :: StateT TcState
          ( Except TcError
          ) a
  }
  deriving newtype ( Functor, Applicative, Monad
                   , MonadState TcState
                   , MonadError TcError
                   )

newUTyVar :: TcM TyVar
newUTyVar = do
  counter <- tvCounter <<+= 1
  pure . UnivTV . Name $ "t" <> tshow counter

normalizeTyp :: Typ -> TcM Typ
normalizeTyp t@(VarT a) = fromMaybe t <$> use (tyVars . at a)
normalizeTyp t          = pure t

--------------------------------------------------------------------------------

unify :: Typ -> Typ -> TcM Typ
unify t@(VarT a) (VarT b)
  | a == b
  = pure t
  | otherwise
  = runReturn $ do
      mATy <- use $ tyVars . at a
      for_ mATy $ \ty -> do
        tyVars . at b ?= ty
        return ty

      mBTy <- use $ tyVars . at b
      for_ mBTy $ \ty -> do
        tyVars . at a ?= ty
        return ty

      pure t
unify (VarT v) t = do
  tyVars . at v ?= t
  pure t
unify t (VarT v) = do
  tyVars . at v ?= t
  pure t
unify t@(ConT a) (ConT b) | a == b = pure t
unify (AppT f1 x1) (AppT f2 x2) = do
  f <- unify f1 f2
  x <- unify x1 x2
  pure $ AppT f x
unify ArrT ArrT = pure ArrT
unify t@(TupleT n1) (TupleT n2) | n1 == n2 = pure t
unify t1 t2 = throwError . fromString
  $ "Couldn't match expected type " <> show t1
  <> " with actual type "            <> show t2

--------------------------------------------------------------------------------

integerT :: Typ
integerT = ConT "Integer"

charT :: Typ
charT = ConT "Char"

stringT :: Typ
stringT = ConT "String"

boolT :: Typ
boolT = ConT "Bool"

tcLit :: Lit -> TcM Typ
tcLit (CharL _)    = pure charT
tcLit (StringL _)  = pure stringT
tcLit (IntegerL _) = pure integerT

--------------------------------------------------------------------------------

tcExpr :: Expr -> TcM Typ
tcExpr (LitE lit) = tcLit lit
tcExpr (SigE e t) = unify t =<< tcExpr e
tcExpr (AppE f x) = tcApp tcExpr f x
tcExpr (TupE es)  = do
  ets <- traverse tcExpr es
  pure $ foldl' AppT (TupleT $ length ets) ets
tcExpr (CondE cond true false) = do
  void $ unify boolT =<< tcExpr cond
  trueT  <- tcExpr true
  falseT <- tcExpr false
  unify trueT falseT
tcExpr (VarE n) = do
  tv <- VarT <$> newUTyVar
  fromMaybe tv <$> (vars . at n <<?= tv)
tcExpr (LetE decs e) = snapshotState $ do
  for_ decs $ \case
    FunD n clauses -> do
      funTy <- tcExpr $ VarE n

      for_ clauses $ \(Clause pats ex) -> do
        let clauseEx = case pats of
              VNil    -> ex
              p :< ps -> LamE $ (p :| Vec.toList ps)
        clauseT <- tcExpr clauseEx
        traceShowM clauseT
        unify funTy clauseT

    TySynD name vars typ -> pure undefined

  normalizeTyp =<< tcExpr e


--------------------------------------------------------------------------------

typeT :: Typ
typeT = ConT "Type"

tcTyp :: Typ -> TcM Typ
tcTyp ArrT       = pure $ typeT :->: typeT :->: typeT
tcTyp (AppT f x) = tcApp tcTyp f x
tcTyp (VarT _)   = VarT <$> newUTyVar -- TODO remember the kind!
tcTyp (ConT _)   = VarT <$> newUTyVar
tcTyp (TupleT n) = pure $ foldr (:->:) typeT $ replicate @[_] n typeT

--------------------------------------------------------------------------------

tcApp :: (a -> TcM Typ) -> a -> a -> TcM Typ
tcApp tcTerm f x = do
  fT <- tcTerm f
  xT <- tcTerm x

  resVar <- newUTyVar

  _ :->: res <- unify fT $ xT :->: VarT resVar

  pure res

--------------------------------------------------------------------------------

runTc :: TcM a -> Either TcError a
runTc = runExcept . flip evalStateT mempty . unTcM
