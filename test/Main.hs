{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

------------------------------------------------------------------------------
import Test.Hspec
import Test.Hspec.Core.Spec (Result (..))
import Test.Hspec.Snap
------------------------------------------------------------------------------
--import           Control.Lens
--import           Control.Monad.IO.Class (liftIO)
import           Control.Monad (void)
import           Snap (route)
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Session.Backends.CookieSession
------------------------------------------------------------------------------
import Site
import Application
import Model.Project
import Model.Item
------------------------------------------------------------------------------

testApp :: SnapletInit App App
testApp = makeSnaplet "app" "Test application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    a <- nestSnaplet "auth" auth $
           initJsonFileAuthManager defAuthSettings sess "users.json"
    d <- nestSnaplet "pg" db pgsInit
    addAuthSplices h auth
    return $ App h s a d

evalDb :: Handler App Postgres a -> SnapHspecM App a
evalDb = eval . with db

clearDb :: Handler App App ()
clearDb = do
  _ <- execute "delete from items" ()
  _ <- execute "delete from projects" ()
  return ()

testProject :: Handler App Postgres (Maybe Integer)
testProject = insertProject "Test project" "Short description"

main :: IO ()
main = hspec $ snap (route routes) testApp $ do

  let failure s = setResult $ Fail Nothing s

  afterEval (void clearDb) $ do

    describe "Model.Item" $ do
      describe "queryItemsAll" $ do
        it "is an empty query to begin with" $ do
          items <- evalDb queryItemsAll
          items `shouldEqual` []

        it "returns an item we've inserted" $ do
          ret <- evalDb testProject
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
            (_x:_xs) -> setResult $ Fail Nothing "Found multiple projects, expecting one"
            _      -> setResult $ Fail Nothing "Found no projects, expecting one"
