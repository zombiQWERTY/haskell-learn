module Db where

import Control.Exception (SomeException, try)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Int
import Data.Pool (Pool, withResource)
import Data.Word (Word16)
import Database.PostgreSQL.Simple
import qualified Database.PostgreSQL.Simple as P
import Domain
import GHC.Generics (Generic)

data DbConfig = DbConfig
  { dbHost :: String,
    dbPort :: Word16,
    dbName :: String,
    dbUser :: String,
    dbPassword :: String
  }
  deriving (Show, Generic)

newConn :: DbConfig -> IO Connection
newConn conf =
  connect
    defaultConnectInfo
      { connectHost = dbHost conf,
        connectPort = dbPort conf,
        connectUser = dbUser conf,
        connectPassword = dbPassword conf,
        connectDatabase = dbName conf
      }

--------------------------------------------------------------------------------
-- Utilities for interacting with the DB.
-- No transactions.
--
-- Accepts arguments
fetch :: (ToRow q, FromRow r) => Pool P.Connection -> q -> Query -> IO (Either SomeException [r])
fetch pool args sql = try $ withResource pool retrieve
  where
    retrieve conn = query conn sql args

-- No arguments -- just pure sql
fetchSimple :: (FromRow r) => Pool P.Connection -> Query -> IO (Either SomeException [r])
fetchSimple pool sql = try $ withResource pool retrieve
  where
    retrieve conn = query_ conn sql

-- Update database
execSql :: ToRow q => Pool P.Connection -> q -> Query -> IO (Either SomeException Int64)
execSql pool args sql = try $ withResource pool ins
  where
    ins conn = execute conn sql args

--------------------------------------------------------------------------------

listArticles :: Pool Connection -> IO (Either SomeException [Article])
listArticles pool = do
  res <- fetchSimple pool "SELECT * FROM articles ORDER BY id DESC"
  pure $ bimap id (map (\(id, title, content) -> Article id title content)) res
