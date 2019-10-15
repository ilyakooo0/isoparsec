import qualified Spec.Megaparsec.BasicNums as BasicNums
import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
  spec <- testSpec "Basic megaparsec test" BasicNums.spec
  defaultMain
    ( testGroup
        "tests"
        [ spec
        ]
    )
