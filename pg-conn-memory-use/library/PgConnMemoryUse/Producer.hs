{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module PgConnMemoryUse.Producer
  ( produceNotifications
  ) where

import Control.Exception (bracket)
import Control.Monad (forever, void)
import Data.Foldable (for_)
import Data.String (IsString(fromString))
import Data.Text (Text)
import Database.PostgreSQL.Simple (ConnectInfo, Only, close, connect, execute, query_)

produceNotifications :: ConnectInfo -> IO ()
produceNotifications connInfo = do
  bracket (connect connInfo) close \conn -> do
    forever do
      tableNames :: [Only Text] <- do
        query_ conn $ fromString $ unwords
          [ "SELECT table_schema || '.' || table_name"
          , "from information_schema.tables"
          , "where table_schema = 'information_schema'"
          ]
      for_ tableNames \tableName -> do
        void $ execute conn "NOTIFY demo_notification, ?" tableName
