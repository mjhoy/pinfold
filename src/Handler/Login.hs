{-# LANGUAGE OverloadedStrings #-}

module Handler.Login
       ( handleLogin
       , handleLogout
       , currentAdmin
       ) where

------------------------------------------------------------------------------
import           Data.Monoid
import           Control.Applicative
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist
import           Heist
import           Data.Readable
import qualified Data.Text.Encoding as TE
import qualified Heist.Interpreted as I
------------------------------------------------------------------------------
import           Application
import           Model.Admin


------------------------------------------------------------------------------
-- | Move from Snaplet.Auth's internal text representation to an integer
uidToInt :: UserId -> Maybe Integer
uidToInt uid = fromText $ unUid uid


------------------------------------------------------------------------------
-- | Return the current Admin
currentAdmin :: Handler App (AuthManager App) (Maybe Admin)
currentAdmin = do
  curUser <- currentUser
  case curUser of
    (Just u) -> case userId u of
      (Just uid) -> do
        case uidToInt uid of
          (Just uid') -> do
             withTop db $ adminForAuthUser uid'
          Nothing -> logError ("Error parsing uid: " <> (TE.encodeUtf8 (unUid uid))) >> return Nothing
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
              let uid' = uidToInt uid
              case uid' of
                Nothing -> do
                  logError $ "error parsing userId: " <> (TE.encodeUtf8 (unUid uid))
                  redirect "/"
                (Just uid'') -> do
                  res <- withTop db $ ensureAdminFromAuthUser uid''
                  case res of
                    Just _x -> redirect "/"
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
