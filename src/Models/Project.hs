module Models.Project
  ( Project(..)
  ) where

import qualified Data.Text as T
import           Database.PostgreSQL.Simple.FromRow

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
