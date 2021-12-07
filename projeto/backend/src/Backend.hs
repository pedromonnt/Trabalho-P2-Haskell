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

migrateLivr :: Query
migrateLivr = "CREATE TABLE IF NOT EXISTS livroo (id SERIAL PRIMARY KEY, titulo TEXT NOT NULL, autor TEXT NOT NULL, editora TEXT NOT NULL, categoria TEXT NOT NULL, valor REAL NOT NULL, qt INTEGER NOT NULL)"

migrateFunc :: Query
migrateFunc = "CREATE TABLE IF NOT EXISTS funcionarioo (id SERIAL PRIMARY KEY, nome TEXT NOT NULL, cargo TEXT NOT NULL, email TEXT NOT NULL, telefone TEXT NOT NULL, salario REAL NOT NULL)"

migrateComp :: Query
migrateComp = "CREATE TABLE IF NOT EXISTS compradorr (id SERIAL PRIMARY KEY, nome TEXT NOT NULL, endereco TEXT NOT NULL, cidade TEXT NOT NULL, estado TEXT NOT NULL, email TEXT NOT NULL)"

backend :: Backend BackendRoute FrontendRoute
backend = Backend
   { _backend_run = \serve -> do
      dbcon <- connect getConn
      serve $ do
         \case
            BackendRoute_ListarLivr :/ () -> method GET $ do
               res :: [Livro] <- liftIO $ do
                  execute_ dbcon migrateLivr
                  query_ dbcon "SELECT * from livroo"
               modifyResponse $ setResponseStatus 200 "OK"
               writeLazyText (encodeToLazyText res)
            
            BackendRoute_ListarFunc :/ () -> method GET $ do
               res :: [Funcionario] <- liftIO $ do
                  execute_ dbcon migrateFunc
                  query_ dbcon "SELECT * from funcionarioo"
               modifyResponse $ setResponseStatus 200 "OK"
               writeLazyText (encodeToLazyText res)
            
            BackendRoute_ListarComp :/ () -> method GET $ do
               res :: [Comprador] <- liftIO $ do
                  execute_ dbcon migrateComp
                  query_ dbcon "SELECT * from compradorr"
               modifyResponse $ setResponseStatus 200 "OK"
               writeLazyText (encodeToLazyText res)
            
            BackendRoute_DeletarLivr :/ pid -> method DELETE $ do
               res :: [Livro] <- liftIO $ do
                  execute_ dbcon migrateLivr
                  query dbcon "DELETE from livroo WHERE id=?" (Only (pid :: Int))
               if res /= [] then do
                  modifyResponse $ setResponseStatus 200 "OK"
                  writeLazyText (encodeToLazyText (Prelude.head res))
               else
                  modifyResponse $ setResponseStatus 404 "NOT FOUND"
            
            BackendRoute_DeletarFunc :/ pid -> method DELETE $ do
               res :: [Funcionario] <- liftIO $ do
                  execute_ dbcon migrateFunc
                  query dbcon "DELETE from funcionarioo WHERE id=?" (Only (pid :: Int))
               if res /= [] then do
                  modifyResponse $ setResponseStatus 200 "OK"
                  writeLazyText (encodeToLazyText (Prelude.head res))
               else
                  modifyResponse $ setResponseStatus 404 "NOT FOUND"
            
            BackendRoute_DeletarComp :/ pid -> method DELETE $ do
               res :: [Comprador] <- liftIO $ do
                  execute_ dbcon migrateComp
                  query dbcon "DELETE from compradorr WHERE id=?" (Only (pid :: Int))
               if res /= [] then do
                  modifyResponse $ setResponseStatus 200 "OK"
                  writeLazyText (encodeToLazyText (Prelude.head res))
               else
                  modifyResponse $ setResponseStatus 404 "NOT FOUND"

            BackendRoute_EditarLivr :/ pid -> method POST $ do
               livr <- A.decode <$> readRequestBody 2000
               case livr of
                  Just livro -> do
                     liftIO $ do
                        execute_ dbcon migrateLivr
                        execute dbcon "UPDATE livroo SET titulo = ?, autor = ?, editora = ?, categoria = ?, valor = ?, qt = ? WHERE id = ?"
                           (livroTitulo livro, livroAutor livro, livroEditora livro, livroCategoria livro, livroValor livro, livroQt livro, pid)
                     modifyResponse $ setResponseStatus 200 "OK"
                  Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"
            
            BackendRoute_EditarFunc :/ pid -> method POST $ do
               func <- A.decode <$> readRequestBody 2000
               case func of
                  Just funcionario -> do
                     liftIO $ do
                        execute_ dbcon migrateFunc
                        execute dbcon "UPDATE funcionarioo SET nome = ?, cargo = ?, email = ?, telefone = ?, salario = ? WHERE id = ?"
                           (funcionarioNome funcionario, funcionarioCargo funcionario, funcionarioEmail funcionario, funcionarioTelefone funcionario, funcionarioSalario funcionario, pid)
                     modifyResponse $ setResponseStatus 200 "OK"
                  Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"
            
            BackendRoute_EditarComp :/ pid -> method POST $ do
               comp <- A.decode <$> readRequestBody 2000
               case comp of
                  Just comprador -> do
                     liftIO $ do
                        execute_ dbcon migrateComp
                        execute dbcon "UPDATE compradorr SET nome = ?, endereco = ?, cidade = ?, estado = ?, email = ? WHERE id = ?"
                           (compradorNome comprador, compradorEndereco comprador, compradorCidade comprador, compradorEstado comprador, compradorEmail comprador, pid)
                     modifyResponse $ setResponseStatus 200 "OK"
                  Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"
            
            BackendRoute_BuscarLivr :/ pid -> method GET $ do
               res :: [Livro] <- liftIO $ do
                  execute_ dbcon migrateLivr
                  query dbcon "SELECT * from livroo WHERE id=?" (Only (pid :: Int))
               if res /= [] then do
                  modifyResponse $ setResponseStatus 200 "OK"
                  writeLazyText (encodeToLazyText (Prelude.head res))
               else
                  modifyResponse $ setResponseStatus 404 "NOT FOUND"
            
            BackendRoute_BuscarFunc :/ pid -> method GET $ do
               res :: [Funcionario] <- liftIO $ do
                  execute_ dbcon migrateFunc
                  query dbcon "SELECT * from funcionarioo WHERE id=?" (Only (pid :: Int))
               if res /= [] then do
                  modifyResponse $ setResponseStatus 200 "OK"
                  writeLazyText (encodeToLazyText (Prelude.head res))
               else
                  modifyResponse $ setResponseStatus 404 "NOT FOUND"
            
            BackendRoute_BuscarComp :/ pid -> method GET $ do
               res :: [Comprador] <- liftIO $ do
                  execute_ dbcon migrateComp
                  query dbcon "SELECT * from compradorr WHERE id=?" (Only (pid :: Int))
               if res /= [] then do
                  modifyResponse $ setResponseStatus 200 "OK"
                  writeLazyText (encodeToLazyText (Prelude.head res))
               else
                  modifyResponse $ setResponseStatus 404 "NOT FOUND"
                      
            BackendRoute_Livro :/ () -> method POST $ do
               livr <- A.decode <$> readRequestBody 2000
               case livr of
                  Just livro -> do
                     liftIO $ do
                        execute_ dbcon migrateLivr
                        execute dbcon "INSERT INTO livroo (titulo,autor,editora,categoria,valor,qt) VALUES (?,?,?,?,?,?)"
                                 (livroTitulo livro, livroAutor livro, livroEditora livro, livroCategoria livro, livroValor livro, livroQt livro)
                     modifyResponse $ setResponseStatus 200 "OK"
                  Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"
            
            BackendRoute_Funcionario :/ () -> method POST $ do
               func <- A.decode <$> readRequestBody 2000
               case func of
                  Just funcionario -> do
                     liftIO $ do
                        execute_ dbcon migrateFunc
                        execute dbcon "INSERT INTO funcionarioo (nome,cargo,email,telefone,salario) VALUES (?,?,?,?,?)"
                                 (funcionarioNome funcionario, funcionarioCargo funcionario, funcionarioEmail funcionario, funcionarioTelefone funcionario, funcionarioSalario funcionario)
                     modifyResponse $ setResponseStatus 200 "OK"
                  Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"
            
            BackendRoute_Comprador :/ () -> method POST $ do
               compr <- A.decode <$> readRequestBody 2000
               case compr of
                  Just comprador -> do
                     liftIO $ do
                        execute_ dbcon migrateComp
                        execute dbcon "INSERT INTO compradorr (nome,endereco,cidade,estado,email) VALUES (?,?,?,?,?)"
                                 (compradorNome comprador, compradorEndereco comprador, compradorCidade comprador, compradorEstado comprador, compradorEmail comprador)
                     modifyResponse $ setResponseStatus 200 "OK"
                  Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"

   , _backend_routeEncoder = fullRouteEncoder
   }
