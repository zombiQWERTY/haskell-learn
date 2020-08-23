{-# LANGUAGE DeriveAnyClass #-}

module Domain where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text.Lazy
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple.FromRow (FromRow)

data Article = Article
  { articleId :: Integer,
    title :: Text,
    content :: Text
  }
  deriving (Show, Generic)

instance ToJSON Article

instance FromJSON Article

instance FromRow Article