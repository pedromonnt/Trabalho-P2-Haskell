{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
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
import Common.Api
import Data.Aeson.Text

getConn :: ConnectInfo
getConn = ConnectInfo "ec2-34-236-136-215.compute-1.amazonaws.com"
                      5432
                      "bnjskrkjxghefj"
      "289819f6c0eaf7835aad2e6d056bd430aaa494ef288616fe0d3951a48fa46a50"
                      "d70lsn5j05kpiq"

migration :: Query
migration = "CREATE TABLE IF NOT EXISTS cliente (id SERIAL PRIMARY KEY, nome TEXT NOT NULL)"

migrateProd :: Query
migrateProd = "CREATE TABLE IF NOT EXISTS produtoo (id SERIAL PRIMARY KEY, nome TEXT NOT NULL, valor REAL NOT NULL, qt INTEGER NOT NULL)"

backend :: Backend BackendRoute FrontendRoute
backend = Backend
   { _backend_run = \serve -> do
      dbcon <- connect getConn
      serve $ do
         \case
            BackendRoute_Listar :/ () -> method GET $ do
               res :: [Produto] <- liftIO $ do
                  execute_ dbcon migrateProd
                  query_ dbcon "SELECT * from produtoo"
               modifyResponse $ setResponseStatus 200 "OK"
               writeLazyText (encodeToLazyText res)
            BackendRoute_Buscar :/ pid -> method GET $ do
               res :: [Produto] <- liftIO $ do
                  execute_ dbcon migrateProd
                  query dbcon "SELECT * from produtoo WHERE id=?" (Only (pid :: Int))
               if res /= [] then do
                  modifyResponse $ setResponseStatus 200 "OK"
                  writeLazyText (encodeToLazyText (Prelude.head res))
               else
                  modifyResponse $ setResponseStatus 404 "NOT FOUND"
            BackendRoute_Produto :/ () -> method POST $ do
               prod <- A.decode <$> readRequestBody 2000
               case prod of
                  Just produto -> do
                     liftIO $ do
                        execute_ dbcon migrateProd
                        execute dbcon "INSERT INTO produtoo (nome,valor,qt) VALUES (?,?,?)"
                                 (produtoNome produto, produtoValor produto, produtoQt produto)
                     modifyResponse $ setResponseStatus 200 "OK"
                  Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"
            BackendRoute_Cliente :/ () -> method POST $ do
               Just nome <- A.decode <$> readRequestBody 2000
               liftIO $ do
                  execute_ dbcon migration
                  execute dbcon "INSERT INTO cliente (nome) VALUES (?)" [nome :: Text]
               modifyResponse $ setResponseStatus 200 "OK"
            _ -> return ()
   , _backend_routeEncoder = fullRouteEncoder
   }
