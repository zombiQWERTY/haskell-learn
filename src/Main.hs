{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.Pool (Pool, createPool)
import Data.Word (Word16)
import Database.PostgreSQL.Simple (Connection, close, withTransaction)
import Database.PostgreSQL.Simple.Migration
import Db
import Handler
import Web.Scotty (scotty)

makeDbConfig :: C.Config -> IO (Maybe Db.DbConfig)
makeDbConfig conf = do
  host <- C.lookup conf "database.host" :: IO (Maybe String)
  port <- C.lookup conf "database.port" :: IO (Maybe Word16)
  name <- C.lookup conf "database.name" :: IO (Maybe String)
  user <- C.lookup conf "database.user" :: IO (Maybe String)
  password <- C.lookup conf "database.pass" :: IO (Maybe String)

  pure $
    DbConfig <$> host
      <*> port
      <*> name
      <*> user
      <*> password

makeMigrations :: IO (Connection) -> FilePath -> IO (MigrationResult String)
makeMigrations conn dir = do
  c <- conn
  withTransaction c $
    runMigrations True c [MigrationInitialization, MigrationDirectory dir]

makeDbPool :: IO Connection -> IO (Pool Connection)
makeDbPool conn = createPool conn close 1 64 10

main :: IO ()
main = do
  loadedConf <- C.load [C.Required "application.conf"]
  dbConf <- makeDbConfig loadedConf

  case dbConf of
    Nothing -> putStrLn "No database configuration found, terminating..."
    Just conf -> do
      makeMigrations conn "./migrations"
      pool <- makeDbPool conn
      scotty 3000 $ makeHandlers pool
      where
        conn = newConn conf
