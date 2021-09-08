
import Test.Tasty

import Bugs ( bugs )
import Features ( features )

main :: IO ()
main = do
  defaultMain $ testGroup "All"
    [ testGroup "Bugs" bugs
    , testGroup "Features" features
    ]
