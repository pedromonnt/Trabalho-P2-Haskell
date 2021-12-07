{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Common.Route where

{- -- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category
-}

import Data.Text (Text, unpack)
import Data.Functor.Identity
import Data.Function
import Obelisk.Route
import Obelisk.Route.TH

checFullREnc :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
checFullREnc = checkEncoder fullRouteEncoder &
   \case
      Left err -> error $ unpack err
      Right encoder -> encoder

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing     :: BackendRoute ()
  BackendRoute_Livro       :: BackendRoute ()
  BackendRoute_Funcionario :: BackendRoute ()
  BackendRoute_Comprador   :: BackendRoute ()
  BackendRoute_ListarLivr  :: BackendRoute ()
  BackendRoute_ListarFunc  :: BackendRoute ()
  BackendRoute_ListarComp  :: BackendRoute ()
  BackendRoute_BuscarLivr  :: BackendRoute Int
  BackendRoute_BuscarFunc  :: BackendRoute Int
  BackendRoute_BuscarComp  :: BackendRoute Int
  BackendRoute_EditarLivr  :: BackendRoute Int
  BackendRoute_EditarFunc  :: BackendRoute Int
  BackendRoute_EditarComp  :: BackendRoute Int
  BackendRoute_DeletarLivr :: BackendRoute Int
  BackendRoute_DeletarFunc :: BackendRoute Int
  BackendRoute_DeletarComp :: BackendRoute Int
  -- You can define any routes that will be handled specially by the backend here.
  -- i.e. These do not serve the frontend, but do something different, such as serving static files.

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
  -- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing     -> PathSegment "missing" $ unitEncoder mempty
      BackendRoute_Livro       -> PathSegment "livro" $ unitEncoder mempty
      BackendRoute_Funcionario -> PathSegment "funcionario" $ unitEncoder mempty
      BackendRoute_Comprador   -> PathSegment "comprador" $ unitEncoder mempty
      BackendRoute_ListarLivr  -> PathSegment "lislivr" $ unitEncoder mempty
      BackendRoute_ListarFunc  -> PathSegment "lisfunc" $ unitEncoder mempty
      BackendRoute_ListarComp  -> PathSegment "liscomp" $ unitEncoder mempty
      BackendRoute_BuscarLivr  -> PathSegment "buslivr" readShowEncoder
      BackendRoute_BuscarFunc  -> PathSegment "busfunc" readShowEncoder
      BackendRoute_BuscarComp  -> PathSegment "buscomp" readShowEncoder
      BackendRoute_EditarLivr  -> PathSegment "edilivr" readShowEncoder
      BackendRoute_EditarFunc  -> PathSegment "edifunc" readShowEncoder
      BackendRoute_EditarComp  -> PathSegment "edicomp" readShowEncoder
      BackendRoute_DeletarLivr -> PathSegment "dellivr" readShowEncoder
      BackendRoute_DeletarFunc -> PathSegment "delfunc" readShowEncoder
      BackendRoute_DeletarComp -> PathSegment "delcomp" readShowEncoder
      )
  (\case
      FrontendRoute_Main -> PathEnd $ unitEncoder mempty)

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]
