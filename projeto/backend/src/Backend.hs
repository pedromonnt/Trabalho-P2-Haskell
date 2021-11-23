{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase, GADTs #-}
module Backend where

import Common.Route
import Obelisk.Backend
import Database.PostgreSQL.Simple
import Data.Text
import Obelisk.Route
import Snap.Core
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A

getConn :: ConnectInfo
getConn = ConnectInfo "ec2-34-236-136-215.compute-1.amazonaws.com"
                      5432
                      "bnjskrkjxghefj"
      "289819f6c0eaf7835aad2e6d056bd430aaa494ef288616fe0d3951a48fa46a50"
                      "d70lsn5j05kpiq"

migration :: Query
migration = "CREATE TABLE IF NOT EXISTS cliente (id SERIAL PRIMARY KEY, nome TEXT NOT NULL)"

backend :: Backend BackendRoute FrontendRoute
backend = Backend
   { _backend_run = \serve -> do
      dbcon <- connect getConn
      serve $ do
         \case
            BackendRoute_Cliente :/ () -> do
               Just nome <- A.decode <$> readRequestBody 2000
               liftIO $ do
                  execute_ dbcon migration
                  execute dbcon "INSERT INTO cliente (nome) VALUES (?)" [nome :: Text]
               modifyResponse $ setResponseStatus 200 "OK"
            _ -> return ()
   , _backend_routeEncoder = fullRouteEncoder
   }
