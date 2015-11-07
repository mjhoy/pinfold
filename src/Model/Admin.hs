{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Model.Admin
       ( Admin(..)
       , insertAdminFromAuthUser
       , ensureAdminFromAuthUser
       , adminForAuthUser
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

adminForAuthUser :: HasPostgres m =>
                    Integer ->
                    m (Maybe Admin)
adminForAuthUser uid = do
    ret <- query sql args
    case ret of
      (x:_xs) -> return $ Just x
      _       -> return Nothing
  where
    sql  = "select aid, uid from admins where uid = ? limit 1"
    args = (Only uid)

insertAdminFromAuthUser :: HasPostgres m =>
                           Integer ->        -- authUser uid
                           m (Maybe Integer) -- new admin aid
insertAdminFromAuthUser uid = do
    [Only aid] <- query sql args
    return $ Just aid
  where
    sql  = "insert into admins (uid) values (?) returning aid"
    args = Only uid

ensureAdminFromAuthUser :: HasPostgres m =>
                           Integer ->        -- authUser uid
                           m (Maybe Integer) -- new admin aid
ensureAdminFromAuthUser uid = do
    r <- query "select aid from admins where uid = ?" (Only uid)
    case r of
      [] -> insertAdminFromAuthUser uid
      [Only aid] -> return $ Just aid
      _ -> do
        return Nothing
