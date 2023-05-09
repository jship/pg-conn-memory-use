module Main
  ( main
  ) where

import Database.PostgreSQL.Simple (defaultConnectInfo)
import PgConnMemoryUse (produceNotifications)

main :: IO ()
main = produceNotifications defaultConnectInfo
