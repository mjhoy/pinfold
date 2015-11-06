{-# LANGUAGE OverloadedStrings #-}

module Handler.Login
       ( handleLogin
       , handleLogout
       , currentAdmin
       ) where

------------------------------------------------------------------------------
import           Data.Monoid
import           Control.Applicative
import           Data.ByteString (pack)
import           Control.Monad.State.Lazy (liftIO)
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist
import           Snap.Snaplet.PostgresqlSimple
import           Heist
import qualified Data.Text as T
import qualified Heist.Interpreted as I
import qualified Data.ByteString.Char8 as B8
------------------------------------------------------------------------------
import           Application
import           Model.Admin


------------------------------------------------------------------------------
-- | Move from Snaplet.Auth's internal text representation to an integer
--
-- This is somewhat unsafe, however if you look in the postgres backend
-- you see it is basically the reverse of how userId is modified to be
-- stored in the database.
uidToInt :: UserId -> Integer
uidToInt uid = read $ T.unpack $ unUid uid


------------------------------------------------------------------------------
-- | Return the current Admin
currentAdmin :: Handler App (AuthManager App) (Maybe Admin)
currentAdmin = do
  curUser <- currentUser
  case curUser of
    (Just u) -> case userId u of
      (Just uid) -> do
        withTop db $ adminForAuthUser $ uidToInt uid
      Nothing -> return Nothing
    Nothing -> return Nothing


------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Handler App (AuthManager App) ()
handleLogin = method GET (render "login") <|> method POST handleLoginSubmit


handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit = loginUser "login" "password" Nothing failure success
  where
    -- Login succeeded
    success = do
      maybeCurUser <- currentUser
      case maybeCurUser of
        (Just curUser) -> do
          case userId curUser of
            (Just uid) -> do
              let uid'  = uidToInt uid
              res <- withTop db $ ensureAdminFromAuthUser uid'
              case res of
                Just x -> redirect "/"
                _ -> do
                  logError "no aid found!"
                  redirect "/"
            Nothing -> do
              logError "unexpected error: no user id"
              redirect "/"
        Nothing -> do
          logError "unexpected error: no current user (even though login successful)"
          redirect "/"

    -- Login failed
    failure _ = handleErr $ Just "Unknown user or password"

    handleErr authError =
        heistLocal (I.bindSplices errs) $ render "login"
      where
        errs = maybe mempty splice authError
        splice err = "loginError" ## I.textSplice err


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"
