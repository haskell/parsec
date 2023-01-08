module Features
       ( features
       ) where

import Test.Tasty

import qualified Features.Feature80
import qualified Features.Feature150

features :: [TestTree]
features =
    [ Features.Feature80.main
    , Features.Feature150.main
    ]
