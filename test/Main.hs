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
  _ <- execute "truncate projects" ()
  return ()

main :: IO ()
main = hspec $ snap (route routes) testApp $ do

  afterEval (void clearDb) $ do

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
        
