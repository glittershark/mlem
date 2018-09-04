{-# LANGUAGE OverloadedLists #-}
-- |

module Mlem.TypeCheckTest (main, test) where

import Test.Prelude
import Test.Support.TypeEquality
import Control.Lens
import Mlem.AST
import Mlem.TypeCheck
import Mlem.Data.Vec

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "type checking"
  [ testGroup "well-typed expressions"
    [ testCase "(plus : Integer -> Integer -> Integer) 1 1 : Integer" $
      let expr =
            AppE
              (AppE (SigE (VarE "plus")
                    (ConT "Integer" :->: ConT "Integer" :->: ConT "Integer"))
                (LitE (IntegerL 1)))
              (LitE (IntegerL 1))
      in hasETypeVia NominalEquality expr $ ConT "Integer"
    , testCase "let x = 1 in x : Integer" $
      let expr = LetE [FunD "x" [Clause VNil . LitE $ IntegerL 1]] $ VarE "x"
      in hasETypeVia NominalEquality expr $ ConT "Integer"
    ]
  , testGroup "ill-typed expressions"
    [ testCase "1 2" . illTypedE $ LitE (IntegerL 1) `AppE` LitE (IntegerL 2)
    , testProperty "1 x" $ \expr -> illTypedE $ LitE (IntegerL 1) `AppE` expr
    ]
  ]


--------------------------------------------------------------------------------

hasETypeVia :: (Eq eq, Show eq) => (Typ -> eq) -> Expr -> Typ -> Assertion
hasETypeVia typEq expr typ
  = typEq <$> runTc (tcExpr expr) @?= Right (typEq typ)

hasTTypeVia :: (Eq eq, Show eq) => (Typ -> eq) -> Typ -> Typ -> Assertion
hasTTypeVia typEq typ expected
  = typEq <$> runTc (tcTyp typ) @?= Right (typEq expected)

illTypedE :: Expr -> Assertion
illTypedE expr =
  let result = runTc $ tcExpr expr
  in has _Left result @? show result
