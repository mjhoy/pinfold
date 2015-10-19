{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Helper
       ( testApp
       , evalDb
       , clearDb
       , failure
         ) where

------------------------------------------------------------------------------
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Session.Backends.CookieSession
------------------------------------------------------------------------------
import Application
------------------------------------------------------------------------------
import Test.Hspec.Snap
import Test.Hspec.Core.Spec (Result (..))
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

failure :: String -> SnapHspecM b ()
failure s = setResult $ Fail Nothing s
