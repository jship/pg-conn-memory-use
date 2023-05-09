module Main
  ( main
  ) where

import Database.PostgreSQL.Simple (defaultConnectInfo)
import PgConnMemoryUse (consumeNotifications)

main :: IO ()
main = consumeNotifications defaultConnectInfo
