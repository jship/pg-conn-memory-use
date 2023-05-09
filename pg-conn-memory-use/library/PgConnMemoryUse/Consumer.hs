{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module PgConnMemoryUse.Consumer
  ( consumeNotifications
  ) where

import Control.Exception (bracket)
import Control.Monad (forever, void, when)
import Data.IORef (atomicModifyIORef', newIORef)
import Data.String (IsString(fromString))
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Database.PostgreSQL.Simple
  ( Only(Only), ConnectInfo, Connection, close, connect, execute_, query, withTransaction
  )
import Database.PostgreSQL.Simple.Notification
  ( Notification(Notification, notificationData), getNotification
  )
import Database.PostgreSQL.Simple.Types (QualifiedIdentifier)

consumeNotifications :: ConnectInfo -> IO ()
consumeNotifications connInfo = do
  notifCountRef <- newIORef @Int 1
  bracket (connect connInfo) close \listenConn -> do
    void $ execute_ listenConn "LISTEN demo_notification"
    bracket (connect connInfo) close \conn -> do
      forever do
        Notification { notificationData } <- getNotification listenConn
        withTransaction conn do
          process conn $ fromString $ unpack $ decodeUtf8 notificationData
        notifCount <- atomicModifyIORef' notifCountRef \notifCount ->
          (1 + notifCount, notifCount)
        when (notifCount `mod` 1_000 == 0) do
          putStrLn $ "Processed " <> show notifCount <> " notifications so far!"

process :: Connection -> QualifiedIdentifier -> IO ()
process conn tableName = do
  query conn "SELECT count(*) from ?" (Only tableName) >>= \case
    [] ->
      error "PgConnMemoryUse.Consumer: impossible to have no result rows"
    [Only _notifCount :: Only Int] ->
      pure ()
    _vals ->
      error "PgConnMemoryUse.Consumer: impossible to have more than one result row"
