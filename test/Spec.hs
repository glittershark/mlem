import           Test.Prelude
import qualified Mlem.TypeCheckTest as TypeCheckTest

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Mlem" $
  [ TypeCheckTest.test ]
