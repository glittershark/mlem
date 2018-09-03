{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- module : Mlem.AST
--------------------------------------------------------------------------------
module Mlem.AST
  ( Name(..)
  , Lit(..)
  , Expr(..)
  , Typ(..)
  , TyVar(..)
  , pattern (:->:)
  ) where
--------------------------------------------------------------------------------
import Mlem.Prelude
import Control.Lens
import Data.List.NonEmpty
--------------------------------------------------------------------------------
import Mlem.Data.Vec
--------------------------------------------------------------------------------

newtype Name  where
  Name :: Text -> Name
  deriving stock    (Eq, Show, Generic)
  deriving newtype  (IsString)
  deriving anyclass (Hashable)

data Lit where
  CharL     :: Char     -> Lit
  StringL   :: Text     -> Lit
  IntegerL  :: Integer  -> Lit

data Pat where
  VarP :: Name -> Pat

data Expr :: Type where
  VarE  :: Name                -> Expr
  LitE  :: Lit                 -> Expr
  AppE  :: Expr -> Expr         -> Expr
  LamE  :: NonEmpty Pat        -> Expr
  TupE  :: [Expr]              -> Expr
  CondE :: Expr -> Expr  -> Expr -> Expr
  LetE  :: [Dec]               -> Expr
  SigE  :: Expr -> Typ          -> Expr

data Dec :: Type where
  FunD   :: Name -> NonEmpty (Clause n) -> Dec
  TySynD :: Name -> [Name] -> Typ        -> Dec

data Clause :: Nat -> Type where
  Clause :: Vec n Pat -> Expr -> Clause n

data Typ :: Type where
  VarT   :: TyVar     -> Typ
  AppT   :: Typ -> Typ -> Typ
  ConT   :: Name      -> Typ
  ArrT   ::             Typ
  TupleT :: Int       -> Typ
  deriving stock (Show)

infixl 3 `AppT`

pattern (:->:) :: Typ -> Typ -> Typ
pattern f :->: x = ArrT `AppT` f `AppT` x

infixr 8 :->:

data TyVar :: Type where
  UnivTV       :: Name -> TyVar
  UninferredTV :: Name -> TyVar
  ExistTV      :: Name -> TyVar
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (Hashable)

makeWrapped ''Name
makePrisms ''Lit
makePrisms ''Pat
makePrisms ''Expr
makePrisms ''Dec
