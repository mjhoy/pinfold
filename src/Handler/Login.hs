{-# LANGUAGE OverloadedStrings #-}

module Handler.Login
       ( handleLogin
       , handleLogout
       , currentAdmin
       ) where

------------------------------------------------------------------------------
import           Control.Monad
import           Control.Monad.State
import           Control.Applicative
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist
import           Heist
import           Data.Readable
import qualified Data.Text as T
import qualified Heist.Interpreted as I
import           Snap.Snaplet.Session
------------------------------------------------------------------------------
import           Application
import           Model.Admin
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Get the current session Admin ID
getSessionAdminId :: Handler b SessionManager (Maybe AdminId)
getSessionAdminId = do
  aidT <- getFromSession "__admin_id"
  let aid = aidT >>= fromText
  return $ liftM AdminId aid


------------------------------------------------------------------------------
-- | Set the current user's 'AdminId' in the active session
--
setSessionAdminId :: AdminId -> Handler b SessionManager ()
setSessionAdminId (AdminId i) = setInSession "__admin_id" (T.pack (show i))


------------------------------------------------------------------------------
-- | Move from Snaplet.Auth's internal text representation to an integer
uidToInt :: UserId -> Maybe Integer
uidToInt uid = fromText $ unUid uid


------------------------------------------------------------------------------
-- | Return the current Admin
currentAdmin :: Handler App (AuthManager App) (Maybe Admin)
currentAdmin = do
    s   <- gets session
    aid <- withTop s getSessionAdminId
    case aid of
      Nothing -> currentAdminByCurrentUser
      Just aid' -> do
        admin <- withTop db $ adminForAdminId aid'
        return admin
  where
    currentAdminByCurrentUser :: Handler App (AuthManager App) (Maybe Admin)
    currentAdminByCurrentUser = do
      curUser <- currentUser
      let uid = curUser >>= userId >>= uidToInt
      case uid of
        (Just uid') -> do
          admin <- withTop db $ adminForAuthUser uid'
          case admin of
            Just a -> do
              s <- gets session
              withTop s $ setSessionAdminId $ adminId a
              return $ Just a
            Nothing -> redirect "/"
        Nothing -> do
          logError "Error parsing uid"
          return Nothing

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
      maybeUid <- return $ do
        user <- maybeCurUser
        uid <- userId user
        uidToInt uid
      case maybeUid of
        Nothing -> do
          -- we have supposedly logged in successfuly above, so this
          -- path is only reached unexpectedly. log out and log an
          -- error.
          logout
          logError "unexpected: could not parse uid"
          handleErr $ Just "There was a problem. Please contact site administrator."
        (Just uid) -> do
          res <- withTop db $ ensureAdminFromAuthUser uid
          case res of
            Just aid -> do
              s <- gets session
              withTop s $ setSessionAdminId aid
              redirect "/"
            _ -> do
              logError "no aid found!"
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
