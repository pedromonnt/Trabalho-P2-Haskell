{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Frontend where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Common.Api
import Common.Route


-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.

caixas :: (DomBuilder t m, PostBuild t m) => m ()
caixas = el "div" $ do
  t <- inputElement def -- Dynamic Text
  s <- inputElement def -- Dynamic Text
  text " "
  dynText $ zipDynWith (<>) (_inputElement_value t) (_inputElement_value s)

menu :: DomBuilder t m => m ()
menu = do
  el "div" $ do
    el "ul" $ do
      el "li" (text "Juliana de Oliveira Angotti")
      el "li" (text "Pedro Silva de Sá Monnerat")
      el "li" (text "Renata Fuschini Alaggio")

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Trabalho P2 Haskell"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      el "h1" $ text "Trabalho P2 Haskell"
      caixas
      menu
      el "p" $ text $ T.pack commonStuff
      
      -- `prerender` and `prerender_` let you choose a widget to run on the server
      -- during prerendering and a different widget to run on the client with
      -- JavaScript. The following will generate a `blank` widget on the server and
      -- print "Hello, World!" on the client.
      prerender_ blank $ liftJSM $ void $ eval ("console.log('Hello, World!')" :: T.Text)

      elAttr "img" ("src" =: static @"gerson.jpeg") blank
      el "div" $ do
        exampleConfig <- getConfig "common/example"
        case exampleConfig of
          Nothing -> text "No config file found in config/common/example"
          Just s -> text $ T.decodeUtf8 s
      return ()
  }
