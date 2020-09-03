{-# LANGUAGE DeriveAnyClass #-}

module Handler where

import Control.Exception (Exception (displayException), SomeException)
import Control.Monad.IO.Class
import Data.Aeson hiding (json)
import Data.ByteString.Lazy (ByteString)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Db
import GHC.Generics (Generic)
import Web.Scotty

data ResponseSuccess r = ResponseSuccess {result :: r}
  deriving (Show, Generic, ToJSON)

data ResponseFailure = ResponseFailure {message :: String}
  deriving (Show, Generic, ToJSON)

makeResponse :: (ToJSON r, Show r) => Either SomeException r -> ActionM ByteString
makeResponse d = do
  setHeader "Content-Type" "application/json"
  pure $ either (encode . ResponseFailure . displayException) (encode . ResponseSuccess) d

makeHandlers :: Pool Connection -> ScottyM ()
makeHandlers pool = do
  "/articles" `get` (raw =<< makeResponse =<< liftIO (listArticles pool))

