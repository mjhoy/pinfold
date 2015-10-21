{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Model.Admin
       ( Admin(..)
       , insertAdminFromAuthUser
       ) where

--import Snap.Snaplet.Auth (AuthUser(..))
import Snap.Snaplet.PostgresqlSimple

-- todo:
-- a function from AuthUser -> Maybe Admin
-- a function from Admin    -> AuthUser

data Admin = Admin
  { adminId       :: Integer
  , adminUid      :: Integer
  } deriving (Eq, Show)

instance FromRow Admin where
  fromRow = Admin <$> field
                  <*> field

insertAdminFromAuthUser :: HasPostgres m =>
                           Integer ->        -- authUser uid
                           m (Maybe Integer) -- new admin aid
insertAdminFromAuthUser uid = do
    [Only aid] <- query sql args
    return $ Just aid
  where
    sql  = "insert into admins (uid) values (?) returning aid"
    args = Only uid
