{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Model.Project
  ( Project(..)
  , queryProjectsAll
  , insertProject
  ) where

import qualified Data.Text as T
import           Database.PostgreSQL.Simple.FromRow
import           Snap.Snaplet.PostgresqlSimple

data Project = Project
  { projectId :: Integer
  , projectTitle :: T.Text
  , projectDescription :: T.Text
  } deriving (Eq)

instance FromRow Project where
  fromRow = Project <$> field <*> field <*> field

instance Show Project where
  show (Project i t d) =
      "Project { projectId: " ++ show i ++
             ", projectTitle: \"" ++ u t ++
           "\", projectDescription: \"" ++ u d ++
           "\" }"
    where
      u = T.unpack


--------------------------------------------------
-- | SQL queries

queryProjectsAll :: HasPostgres m => m [Project]
queryProjectsAll = query_ "select pid, title, description from projects"

insertProject :: HasPostgres m =>
                 T.Text -> -- Title
                 T.Text -> -- Description
                 m (Maybe Integer) -- a new project id
insertProject t d = do
    [Only r] <- query sql args
    return $ Just r
--    case res of
--      (x:_) -> return $ Just x
--      []    -> return $ Nothing
  where
    sql  = "insert into projects (title, description) values (?,?) returning pid"
    args = (t,d)
