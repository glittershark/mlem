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
  , Dec(..)
  , Clause(..)
  , pattern (:->:)
  ) where
--------------------------------------------------------------------------------
import Mlem.Prelude
import Control.Lens
import Data.List.NonEmpty
import Test.QuickCheck
--------------------------------------------------------------------------------
import Mlem.Data.Vec
import Mlem.Orphans.Arbitrary ()
--------------------------------------------------------------------------------

newtype Name where
  Name :: Text -> Name
  deriving stock    (Eq, Show, Generic)
  deriving newtype  (IsString, Arbitrary)
  deriving anyclass (Hashable)


data Lit where
  CharL     :: Char     -> Lit
  StringL   :: Text     -> Lit
  IntegerL  :: Integer  -> Lit

instance Arbitrary Lit where
  arbitrary = oneof
    [ CharL <$> arbitrary
    , StringL <$> arbitrary
    , IntegerL <$> arbitrary
    ]

data Pat where
  VarP :: Name -> Pat

instance Arbitrary Pat where
  arbitrary = oneof
    [ VarP <$> arbitrary
    ]

data Expr :: Type where
  VarE  :: Name                -> Expr
  LitE  :: Lit                 -> Expr
  AppE  :: Expr -> Expr         -> Expr
  LamE  :: NonEmpty Pat        -> Expr
  TupE  :: [Expr]              -> Expr
  CondE :: Expr -> Expr  -> Expr -> Expr
  LetE  :: [Dec] -> Expr        -> Expr
  SigE  :: Expr -> Typ          -> Expr

data Dec :: Type where
  FunD   :: Name -> NonEmpty (Clause n) -> Dec
  TySynD :: Name -> [Name] -> Typ        -> Dec

instance Arbitrary Dec where
  arbitrary = oneof
    [ FunD <$> arbitrary <*> arbitrary
    , TySynD <$> arbitrary <*> arbitrary <*> arbitrary
    ]

data Clause :: Nat -> Type where
  Clause :: Vec n Pat -> Expr -> Clause n

data Typ :: Type where
  VarT   :: TyVar     -> Typ
  AppT   :: Typ -> Typ -> Typ
  ConT   :: Name      -> Typ
  ArrT   ::             Typ
  TupleT :: Int       -> Typ
  deriving stock (Show)

instance Arbitrary Typ where
  arbitrary = frequency
    [ (5, VarT <$> arbitrary)
    , (1, AppT <$> arbitrary <*> arbitrary)
    , (5, ConT <$> arbitrary)
    , (5, pure ArrT)
    , (5, TupleT <$> arbitrary)
    ]

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

instance Arbitrary TyVar where
  arbitrary = oneof
    [ UnivTV      <$> arbitrary
    , UninferredTV <$> arbitrary
    , ExistTV      <$> arbitrary
    ]

makeWrapped ''Name
makePrisms ''Lit
makePrisms ''Pat
makePrisms ''Expr
makePrisms ''Dec
