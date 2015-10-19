{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Model.ProjectTest where

------------------------------------------------------------------------------
import Test.Hspec
import Test.Hspec.Snap
------------------------------------------------------------------------------
import Test.Helper
import Model.Project
import Application
------------------------------------------------------------------------------


projectTests :: SpecWith (SnapHspecState App)
projectTests = do
  describe "Model.Project" $ do
    describe "queryProjectsAll" $ do

      it "is an empty query to begin with" $ do
        projects <- evalDb queryProjectsAll
        projects `shouldEqual` []

      it "returns a project we've inserted" $ do
        _ <- evalDb $ insertProject "testing" "description"
        projects <- evalDb queryProjectsAll
        case projects of
          (x:[]) -> projectTitle x `shouldEqual` "testing"
          (_x:_xs) -> failure "Found multiple projects, expecting one"
          _        -> failure "Found no projects, expecting one"
