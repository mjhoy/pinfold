{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Model.Project
  ( Project(..)
  , queryProjectsAll
  , insertProject
  , deleteProject
  ) where

import           GHC.Int
import qualified Data.Text as T
import           Database.PostgreSQL.Simple.FromRow
import           Snap.Snaplet.PostgresqlSimple
import qualified Data.ByteString as BS

import Model.Admin

data Project = Project
  { projectId :: Integer
  , projectAid :: AdminId
  , projectTitle :: T.Text
  , projectDescription :: T.Text
  } deriving (Eq, Show)

instance FromRow Project where
  fromRow = Project <$> field
                    <*> field
                    <*> field
                    <*> field

--------------------------------------------------
-- | SQL queries

queryProjectsAll :: HasPostgres m => m [Project]
queryProjectsAll = query_ "select pid, aid, title, description from projects"

insertProject :: HasPostgres m =>
                 BS.ByteString ->  -- Title
                 BS.ByteString ->  -- Description
                 AdminId -> -- aid
                 m (Maybe Integer) -- a new project id
insertProject t d aid = do
    [Only r] <- query sql args
    return $ Just r
  where
    sql  = "insert into projects (title, description, aid) values (?,?,?) returning pid"
    args = (t,d,aid)

deleteProject :: HasPostgres m =>
                 Integer ->
                 m Int64
deleteProject pid = execute "delete from projects where pid = ?" (Only pid)
