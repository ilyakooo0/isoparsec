import qualified Spec.JSON as JSON
import qualified Spec.Megaparsec.BasicNums as BasicNums
import qualified Spec.TwoDigits as TwoDigits
import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
  bsicNumSpec <- testSpec "Basic megaparsec test" BasicNums.spec
  twoDigitsSpec <- testSpec "Two digits test" TwoDigits.spec
  defaultMain
    ( testGroup
        "tests"
        [ testGroup
            "Basic number test"
            [ bsicNumSpec,
              BasicNums.quickSpec
            ],
          -- testGroup
          --   "JSON"
          --   [ JSON.quickSpec
          --   ],
          testGroup
            "TwoDigits"
            [ twoDigitsSpec,
              TwoDigits.quickSpec
            ]
        ]
    )
