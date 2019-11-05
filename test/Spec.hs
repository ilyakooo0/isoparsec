import qualified Spec.JSON as JSON
import qualified Spec.Megaparsec.BasicNums as BasicNums
import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
  spec <- testSpec "Basic megaparsec test" BasicNums.spec
  defaultMain
    ( testGroup
        "tests"
        [ testGroup
            "Basic number test"
            [ spec,
              BasicNums.quickSpec
            ],
          testGroup
            "JSON"
            [ JSON.quickSpec
            ]
        ]
    )
