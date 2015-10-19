module Model.Project
  ( Project(..)
  , queryProjectsAll
  ) where

import qualified Data.Text as T
import           Database.PostgreSQL.Simple.FromRow
import           Snap.Snaplet.PostgresqlSimple

data Project = Project
  { projectId :: Integer
  , projectTitle :: T.Text
  , projectDescription :: T.Text
  }

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
queryProjectsAll = query_ "SELECT id, title, description FROM projects"
