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
import           Control.Applicative
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString (ByteString)
--import qualified Data.Text as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.PostgresqlSimple
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Sass
import           Snap.Util.FileServe
import           Heist
import qualified Heist.Interpreted as I
------------------------------------------------------------------------------
import           Model.Project
------------------------------------------------------------------------------
import           Application


------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Handler App (AuthManager App) ()
handleLogin = method GET handleForm <|> method POST handleFormSubmit
  where
    handleFormSubmit =
        loginUser "login" "password" Nothing (\_ -> handleErr err) (redirect "/")
      where
        err = Just "Unknown user or password"
    handleErr authError =
        heistLocal (I.bindSplices errs) $ render "login"
      where
        errs = maybe mempty splice authError
        splice err = "loginError" ## I.textSplice err
    handleForm = render "login"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"


------------------------------------------------------------------------------
-- | Handle create new project
handleNewProject :: Handler App Postgres ()
handleNewProject = method POST handleFormSubmit
  where
    handleFormSubmit = do
      title <- getPostParam "title"
      description <- getPostParam "description"
      _newProject <- execute "INSERT INTO projects VALUES (?, ?)" (title, description)
      redirect "/"

handleProjects :: Handler App Postgres ()
handleProjects = method GET getAllProjects
  where
    getAllProjects = do
      allProjects <- queryProjectsAll
      liftIO $ print (allProjects :: [Project])

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [
    ------ user --------------------------
    ("/login",    with auth handleLogin)
  , ("/logout",   with auth handleLogout)

    ------ projects ----------------------
  , ("/project/new", with db handleNewProject)
  , ("/projects",    with db handleProjects)

    ------ assets ------------------------
  , ("/sass", with sass sassServe)
  , ("",          serveDirectory "static")

  ]


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
    return $ App h s a d c

