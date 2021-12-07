{-# LANGUAGE DeriveGeneric #-}
{-# language DeriveAnyClass #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Common.Api where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple

commonStuff :: String
commonStuff = "Segue um string definido em Common.Api"

data Livro = Livro {
   livroId :: Int,
   livroTitulo :: Text,
   livroAutor :: Text,
   livroEditora :: Text,
   livroCategoria :: Text,
   livroValor :: Double,
   livroQt :: Int
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)

data Funcionario = Funcionario {
   funcionarioId :: Int,
   funcionarioNome :: Text,
   funcionarioCargo :: Text,
   funcionarioEmail :: Text,
   funcionarioTelefone :: Text,
   funcionarioSalario :: Double
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)

data Comprador = Comprador {
   compradorId :: Int,
   compradorNome :: Text,
   compradorEndereco :: Text,
   compradorCidade :: Text,
   compradorEstado :: Text,
   compradorEmail :: Text
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)
