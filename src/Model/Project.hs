{-# LANGUAGE OverloadedStrings #-}

module Model.Project
  ( Project(..)
  , queryProjectsAll
  , insertProject
  ) where

import qualified Data.Text as T
import           Database.PostgreSQL.Simple.FromRow
import           Snap.Snaplet.PostgresqlSimple
import qualified GHC.Int

data Project = Project
  { projectId :: Integer
  , projectTitle :: T.Text
  , projectDescription :: T.Text
  } deriving (Eq)

instance FromRow Project where
  fromRow = Project <$> field <*> field <*> field

instance Show Project where
  show (Project i t d) =
      "Project { id: " ++ show i ++
             ", title: \"" ++ u t ++
           "\", description: \"" ++ u d ++
           "\" }"
    where
      u = T.unpack


--------------------------------------------------
-- | SQL queries

queryProjectsAll :: HasPostgres m => m [Project]
queryProjectsAll = query_ "select id, title, description from projects"

insertProject :: HasPostgres m =>
                 T.Text -> -- Title
                 T.Text -> -- Description
                 m GHC.Int.Int64
insertProject t d = execute "insert into projects (title, description) values (?,?)" (t,d)

