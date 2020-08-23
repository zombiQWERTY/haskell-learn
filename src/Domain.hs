{-# LANGUAGE DeriveAnyClass #-}

module Domain where

import Data.Text.Lazy
import Database.PostgreSQL.Simple (FromRow)
import GHC.Generics (Generic)

data Article = Article
  { articleId :: Integer,
    title :: Text,
    content :: Text
  }
  deriving (Show, Generic, FromRow)
