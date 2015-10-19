{-# LANGUAGE OverloadedStrings #-}

module Model.Item
       ( Item(..)
       , insertItem
       , queryItemsAll
       ) where

import qualified Data.Text as T
import           Snap.Snaplet.PostgresqlSimple
import qualified GHC.Int

data Item = Item
  { itemId :: Integer
  , itemPath :: FilePath
  , itemCaption :: Maybe T.Text
  , projectId :: Integer
  } deriving (Eq, Show)

instance FromRow Item where
  fromRow = Item <$> field <*> field <*> field <*> field

insertItem :: HasPostgres m =>
              FilePath ->     -- image path
              Maybe T.Text -> -- caption
              Integer ->      -- project id
              m GHC.Int.Int64
insertItem path caption pid = execute sql args
  where sql  = "insert into items (path, caption, pid) values (?,?,?)"
        args = (path, caption, pid)

queryItemsAll :: HasPostgres m => m [Item]
queryItemsAll = query_ "select iid, path, caption, pid from items"
