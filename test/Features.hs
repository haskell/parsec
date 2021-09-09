module Features
       ( features
       ) where

import Test.Tasty

import qualified Features.Feature80

features :: [TestTree]
features = [
             Features.Feature80.main
           ]
