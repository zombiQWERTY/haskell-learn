{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Monad.IO.Class
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.Pool (createPool)
import Data.Word (Word16)
import Database.PostgreSQL.Simple (close, withTransaction)
import Database.PostgreSQL.Simple.Migration
import Db
import Web.Scotty

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

main :: IO ()
main = do
  loadedConf <- C.load [C.Required "application.conf"]
  dbConf <- makeDbConfig loadedConf

  case dbConf of
    Nothing -> putStrLn "No database configuration found, terminating..."
    Just conf -> do
      let dir = "./migrations"
      let default_conn = newConn conf
      migrations_conn <- newConn conf

      withTransaction migrations_conn $
        runMigrations True migrations_conn [MigrationInitialization, MigrationDirectory dir]

      pool <- createPool default_conn close 1 64 10

      scotty 3000 $ do
        get "/articles" $ do
          articles <- liftIO $ listArticles pool
          json articles
