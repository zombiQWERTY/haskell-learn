{-# LANGUAGE DeriveAnyClass #-}

module Domain where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text.Lazy
import Database.PostgreSQL.Simple.FromRow (FromRow)
import GHC.Generics (Generic)

data Article = Article
  { articleId :: Integer,
    title :: Text,
    content :: Text
  }
  deriving (Show, Generic, FromRow, ToJSON, FromJSON)
