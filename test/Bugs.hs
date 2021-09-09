
module Bugs
       ( bugs
       ) where

import Test.Tasty

import qualified Bugs.Bug2
import qualified Bugs.Bug6
import qualified Bugs.Bug9
import qualified Bugs.Bug35

bugs :: [TestTree]
bugs = [ Bugs.Bug2.main
       , Bugs.Bug6.main
       , Bugs.Bug9.main
       , Bugs.Bug35.main
       ]
