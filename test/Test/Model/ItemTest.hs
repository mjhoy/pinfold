{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Model.ItemTest where

------------------------------------------------------------------------------
import           Snap.Snaplet
import           Snap.Snaplet.PostgresqlSimple
------------------------------------------------------------------------------
import Test.Hspec
import Test.Hspec.Snap
------------------------------------------------------------------------------
import Test.Helper
import Model.Item
import Model.Project
import Application
------------------------------------------------------------------------------

testProject :: Integer -> Handler App Postgres (Maybe Integer)
testProject uid = do
  insertProject "Test project" "Short description" uid

itemTests :: SpecWith (SnapHspecState App)
itemTests = do
  describe "Model.Item" $ do
    describe "queryItemsAll" $ do
      it "is an empty query to begin with" $ do
        items <- evalDb queryItemsAll
        items `shouldEqual` []

      it "returns an item we've inserted" $ do
        aid <- eval $ createTestAdmin
        ret <- evalDb $ testProject aid
        case ret of
          Nothing ->
            failure "could not create test project"
          Just pid -> do
            items <- evalDb $ do
              _ <- insertItem "foo.jpg" (Just "caption") pid
              queryItemsAll
            case items of
              (x:[]) -> do
                itemPath x `shouldEqual` "foo.jpg"
                case itemCaption x of
                  Just c -> c `shouldEqual` "caption"
                  Nothing -> failure "item didn't have expected caption"
              (_x:_xs) -> failure "expected just one item; more than one"
              _        -> failure "expected one item; got none"
