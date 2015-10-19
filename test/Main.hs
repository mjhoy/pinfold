{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

------------------------------------------------------------------------------
--import           Control.Lens
--import           Control.Monad.IO.Class (liftIO)
import           Control.Monad (void)
import           Snap (route)
------------------------------------------------------------------------------
import Site
------------------------------------------------------------------------------
import Test.Hspec
import Test.Hspec.Snap
------------------------------------------------------------------------------
import Test.Helper
import Test.Model.ItemTest
import Test.Model.ProjectTest
------------------------------------------------------------------------------

main :: IO ()
main = hspec $ snap (route routes) testApp $ do

  afterEval (void clearDb) $ do

    itemTests
    projectTests
