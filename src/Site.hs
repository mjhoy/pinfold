{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
--
-- The function `routes` is exported for testing.
module Site
       ( app
       , routes
       ) where

------------------------------------------------------------------------------
import           Control.Lens
import           Data.ByteString (ByteString)
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.PostgresqlSimple
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Sass
import           Snap.Util.FileServe
------------------------------------------------------------------------------
import           Heist
import           Heist.Interpreted
import qualified Data.Text as T
import qualified Text.XmlHtml as X
------------------------------------------------------------------------------
import           Handler.Project
import           Handler.Login
------------------------------------------------------------------------------
import           Application


------------------------------------------------------------------------------
-- | Not logged in.
noUserHandler :: Handler App (AuthManager App) ()
noUserHandler = redirect "/"

------------------------------------------------------------------------------
-- | Restricted routes
restricted :: Handler App (AuthManager App) () -> Handler App App ()
restricted h = with auth $ requireUser auth noUserHandler h


------------------------------------------------------------------------------
-- | The overall `content` administration route
-- for now, hand off to the Projects handler
handleContent :: Handler App (AuthManager App) ()
handleContent = handleProjects

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [
    ------ Handler.Login
    ("/login",    with auth handleLogin)
  , ("/logout",   with auth handleLogout)

    ------ Handler.Project
  , ("/projects/new",    restricted handleNewProject)
  , ("/projects/delete", restricted handleDeleteProject)
  , ("/projects",        restricted handleProjects)
  , ("/content",         restricted handleContent)

    ------ Sass assets
  , ("/sass", with sass sassServe)

    ------ Static directory
  , ("",          serveDirectory "static")
  ]

ifBound :: SnapletISplice b
ifBound = do
  inp <- getParamNode
  let t = X.getAttribute "tag" inp
  case t of
    Nothing -> return mempty
    Just t' -> do
      st <- getHS
      let s = lookupSplice t' st
      case s of
        Nothing -> return mempty
        Just _x -> runChildren

addHelperSplices :: HasHeist b =>
                    Snaplet (Heist b) ->
                    Initializer b v ()
addHelperSplices h = addConfig h $ (set scInterpretedSplices $ splices)
                                   mempty
  where splices = do
          "if-bound" ## ifBound

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"

    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    d <- nestSnaplet "pg" db pgsInit

    a <- nestSnaplet "auth" auth $ initPostgresAuth sess d

    c <- nestSnaplet "sass" sass initSass

    addRoutes routes
    addAuthSplices h auth
    addHelperSplices h

--    addConfig h config

    modifyHeistState $ bindSplices $ do
      "if-bound" ## ifBound

    return $ App h s a d c

