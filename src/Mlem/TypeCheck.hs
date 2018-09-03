{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
--------------------------------------------------------------------------------
module Mlem.TypeCheck where
--------------------------------------------------------------------------------
import Mlem.Prelude
import Control.Lens
import Control.Monad.State hiding (return)
import Control.Monad.Except hiding (return)
--------------------------------------------------------------------------------
import Mlem.AST
import Mlem.Util.ShortCircuit
--------------------------------------------------------------------------------

data TcState where
  TcState
    :: { _tyVars :: HashMap TyVar Typ }
    -> TcState
makeLenses ''TcState

instance Semigroup TcState where
  TcState v1 <> TcState v2 = TcState $ v1 <> v2

instance Monoid TcState where
  mempty = TcState mempty

--------------------------------------------------------------------------------

newtype TcError = TcError String
  deriving stock   (Show)
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
newUTyVar = undefined

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
unify t@(ConT a) (ConT b) | a == b = pure t
unify (AppT f1 x1) (AppT f2 x2) =
  unify f1 f2 *> unify x1 x2
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

  resVar <-  newUTyVar
  _ :->: res <- unify fT $ xT :->: VarT resVar

  pure res

--------------------------------------------------------------------------------

runTc :: TcM a -> Either TcError a
runTc = runExcept . flip evalStateT mempty . unTcM
