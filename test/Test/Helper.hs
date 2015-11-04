{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Helper
       ( testApp
       , evalDb
       , clearDb
       , failure
       , createTestAdmin
         ) where

------------------------------------------------------------------------------
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.PostgresqlSimple
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Sass
import           Snap.Snaplet.Session.Backends.CookieSession
import           Data.Text.Encoding (encodeUtf8)
import           Data.Text (unpack)
import           Text.Read (readMaybe)
------------------------------------------------------------------------------
import Application
import Model.Admin
------------------------------------------------------------------------------
import Test.Hspec.Snap
import Test.Hspec.Core.Spec (Result (..))
------------------------------------------------------------------------------


testApp :: SnapletInit App App
testApp = makeSnaplet "app" "Test application." Nothing $ do

  h <- nestSnaplet "" heist $ heistInit "templates"

  s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

  d <- nestSnaplet "pg" db pgsInit

  a <- nestSnaplet "auth" auth $ initPostgresAuth sess d

  c <- nestSnaplet "sass" sass initSass

  addAuthSplices h auth
  return $ App h s a d c

evalDb :: Handler App Postgres a -> SnapHspecM App a
evalDb = eval . with db

clearDb :: Handler App App ()
clearDb = do
  _ <- execute "delete from items" ()
  _ <- execute "delete from projects" ()
  _ <- execute "delete from admins" ()
  _ <- execute "delete from auth_users" ()
  return ()

createTestAdmin :: Handler App App Integer
createTestAdmin = do
  u <- with auth $ createUser "mjh" (encodeUtf8 "pass")
  case u of
    Left e -> error (show e)
    Right user -> case userId user of
      Nothing     -> error ("unexpected: test auth did not have uid")
      Just i      -> do
        case readMaybe (unpack (unUid i)) of
          Nothing  -> error "couldn't create admin"
          Just i' -> do
            res <- with db $ insertAdminFromAuthUser i'
            case res of
              Just aid -> return aid
              Nothing  -> error "couldn't create admin"

failure :: String -> SnapHspecM b ()
failure s = setResult $ Fail Nothing s
