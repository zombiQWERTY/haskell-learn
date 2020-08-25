module Handler where

import Control.Exception (Exception (displayException))
import Control.Monad.IO.Class
import Data.Pool (Pool)
import Data.Text.Lazy (pack)
import Database.PostgreSQL.Simple (Connection)
import Db
import Web.Scotty

makeHandlers :: Pool Connection -> ScottyM ()
makeHandlers pool = do
  get "/articles" $ do
    articles <- liftIO $ listArticles pool
    case articles of
      Left e -> html $ pack $ displayException e
      Right r -> json r
