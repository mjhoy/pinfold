{-# LANGUAGE OverloadedStrings #-}

module Handler.Project
       ( handleNewProject
       , handleProjects
       , handleDeleteProject
       ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Data.Text (pack)
import qualified Data.ByteString.Char8 as B8
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist
import           Heist
import qualified Heist.Interpreted as I
import           Data.Readable
------------------------------------------------------------------------------
import           Application
import           Model.Project
import           Model.Admin
import           Handler.Login
------------------------------------------------------------------------------


splicesFromProject :: Monad n =>
                      Project ->
                      Splices (I.Splice n)
splicesFromProject project = do
  "projectTitle"       ## I.textSplice (projectTitle project)
  "projectDescription" ## I.textSplice (projectDescription project)
  "projectId"          ## I.textSplice (pack $ show $ projectId project)
  "projectAdminId"     ## I.textSplice (pack $ show $ projectAid project)

projectSplices :: [Project] -> Splices (SnapletISplice App)
projectSplices projects = "projects" ## r
  where r = I.mapSplices (I.runChildrenWith . splicesFromProject) projects

------------------------------------------------------------------------------
-- | Handle create new project
handleNewProject :: Handler App (AuthManager App) ()
handleNewProject = method GET handleForm
  where
    handleForm = do
      render "projects/new"


------------------------------------------------------------------------------
-- | Handle delete project
handleDeleteProject :: Handler App (AuthManager App) ()
handleDeleteProject = method GET handleDelete
  where
    handleDelete = do
      pid <- getParam "id"
      case pid >>= fromBS of
        Just pid' -> do
          r <- withTop db $ deleteProject pid'
          case r of
            1 -> redirect "/content" -- Project successfully deleted
            _ -> redirect "/content" -- Project not found
        Nothing -> redirect "/content"


------------------------------------------------------------------------------
-- | Handle projects
-- GET "/content"   -> get all projects index
-- POST "/projects" -> handle the project creation
handleProjects :: Handler App (AuthManager App) ()
handleProjects = method GET handleGetAllProjects <|> method POST handleFormSubmit
  where
    handleGetAllProjects = do
      allProjects <- withTop db $ queryProjectsAll
      renderWithSplices "projects/index" $ projectSplices allProjects
    handleFormSubmit :: Handler App (AuthManager App) ()
    handleFormSubmit = do
        admin <- currentAdmin
        case admin of
          (Just a) -> do
            title <- getPostParam "title"
            description <- getPostParam "description"
            _res <- withTop db $ insertProject' title description (adminId a)
            redirect "/content"
          Nothing -> do
            logError "Couldn't get admin"
            redirect "/projects"
      where
        insertProject' (Just t) (Just d) aid =
          insertProject t d aid
        insertProject' _ _ _ = return $ Nothing

