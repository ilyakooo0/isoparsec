import qualified Spec.BasicReader as BasicReader
import qualified Spec.JSON as JSON
import qualified Spec.JSONHList as JSONHList
import qualified Spec.Megaparsec.BasicNums as BasicNums
import qualified Spec.Ssh as Ssh
import qualified Spec.SshHList as SshHList
import qualified Spec.TwoDigits as TwoDigits
import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
  bsicNumSpec <- testSpec "Basic megaparsec test" BasicNums.spec
  twoDigitsSpec <- testSpec "Two digits test" TwoDigits.spec
  sshSpec <- testSpec "ssh spec" Ssh.spec
  sshHListSpec <- testSpec "ssh HList spec" SshHList.spec
  jsonSpec <- testSpec "json spec" JSON.spec
  jsonHListSpec <- testSpec "json spec" JSONHList.spec
  basicReaderSpec <- testSpec "json spec" BasicReader.spec
  defaultMain
    ( testGroup
        "tests"
        [ testGroup
            "Basic number test"
            [ bsicNumSpec,
              BasicNums.quickSpec
            ],
          testGroup
            "JSON"
            [ jsonSpec,
              JSON.quickSpec
            ],
          testGroup
            "JSONHList"
            [ jsonHListSpec,
              JSONHList.quickSpec
            ],
          testGroup
            "TwoDigits"
            [ twoDigitsSpec,
              TwoDigits.quickSpec
            ],
          testGroup
            "ssh"
            [ sshSpec,
              Ssh.quickSpec
            ],
          testGroup
            "ssh HList"
            [ sshHListSpec,
              SshHList.quickSpec
            ],
          testGroup
            "BasicReader"
            [ basicReaderSpec
            ]
        ]
    )
