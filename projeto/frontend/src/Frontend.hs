{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Frontend where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static
import Data.Map (Map)
import Reflex.Dom.Core
import Text.Read
import Data.Maybe
import Control.Monad.Fix
import Common.Api
import Common.Route
import Data.Aeson

data Pagina = Pagina0 | Pagina1 | Pagina2 | Pagina3 | Pagina4 | Pagina5 | Pagina6

getPath :: R BackendRoute -> T.Text
getPath r = renderBackendRoute checFullREnc r

getListReq :: XhrRequest ()
getListReq = xhrRequest "GET" (getPath (BackendRoute_Listar :/ ())) def

sendRequest :: ToJSON a => R BackendRoute -> a -> XhrRequest T.Text
sendRequest r dados = postJson (getPath r) dados

reqProd :: ( DomBuilder t m
       , Prerender js t m
       ) => m ()
reqProd = do
   nome <- inputElement def
   vl <- numberInput
   qt <- numberInput
   let prod = fmap (\((n,v),q) -> Produto 0 n v q) (zipDyn (zipDyn (_inputElement_value nome) vl) qt)
   (submitBtn,_) <- el' "button" (text "Inserir")
   let click = domEvent Click submitBtn
   let prodEvt = tag (current prod) click
   _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_Produto :/ ()) <$> prodEvt))
   return ()

req :: ( DomBuilder t m
       , Prerender js t m
       ) => m ()
req = do
   inputEl <- inputElement def
   (submitBtn,_) <- el' "button" (text "Inserir")
   let click = domEvent Click submitBtn
   let nm = tag (current $ _inputElement_value inputEl) click
   _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_Cliente :/ ()) <$> nm))
   return ()

tabProduto :: DomBuilder t m => Produto -> m ()
tabProduto pr = do
   el "tr" $ do
      el "td" (text $ T.pack $ show $ produtoId pr)
      el "td" (text $ produtoNome pr)
      el "td" (text $ T.pack $ show $ produtoValor pr)
      el "td" (text $ T.pack $ show $ produtoQt pr)
      
reqLista :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => m ()
reqLista = do
   (btn, _) <- el' "button" (text "Listar")
   let click = domEvent Click btn
   prods :: Dynamic t (Event t (Maybe [Produto])) <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (const getListReq <$> click))
   dynP <- foldDyn (\ps d -> case ps of
                          Nothing -> []
                          Just p -> d++p) [] (switchDyn prods)
   el "table" $ do
      el "thread" $ do
         el "tr" $ do
            el "th" (text "Id")
            el "th" (text "Nome")
            el "th" (text "Valor")
            el "th" (text "Quantidade")
      el "tbody" $ do
         dyn_ (fmap sequence (ffor dynP (fmap tabProduto)))

countClick :: DomBuilder t m => m (Event t Int)
countClick = do
   (ev, _) <- el' "button" (text "+")
   return ((const 1) <$> domEvent Click ev)

pagClick :: (MonadHold t m, DomBuilder t m, PostBuild t m,
            MonadFix m) => m ()
pagClick = do
   evt <- countClick
   st <- accumDyn (+) 0 evt
   el "div" (dynText (fmap (T.pack . show) st))

clickLi :: (DomBuilder t m, PostBuild t m, MonadHold t m) => Pagina -> T.Text -> m (Event t Pagina)
clickLi p t = do
   (ev, _) <- el' "li" (elAttr "a" ("href" =: "#") (text t))
   return ((\_ -> p) <$> domEvent Click ev)

menuLi :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m (Dynamic t Pagina)
menuLi = do
   evs <- el "ul" $ do
      p1 <- clickLi Pagina1 "Exemplo 1: Texto reverso"
      p2 <- clickLi Pagina2 "Exemplo 2: Soma de dois números"
      p3 <- clickLi Pagina3 "Exemplo 3: Contador de cliques"
      p4 <- clickLi Pagina4 "Exemplo 4: Inserção ao banco de dados"
      p5 <- clickLi Pagina5 "Exemplo 5: Inserção de produto"
      p6 <- clickLi Pagina6 "Exemplo 6: Lista de produtos"
      return (leftmost [p1,p2,p3,p4,p5,p6])
   holdDyn Pagina0 evs

currPag :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m, Prerender js t m)
        => Pagina -> m ()
currPag p =
   case p of
      Pagina0 -> blank
      Pagina1 -> bttnEvt
      Pagina2 -> sumEvt
      Pagina3 -> pagClick
      Pagina4 -> req
      Pagina5 -> reqProd
      Pagina6 -> reqLista

mainPag :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m, Prerender js t m) => m ()
mainPag = do
   pag <- el "div" menuLi
   dyn_ $ currPag <$> pag

revText :: T.Text -> T.Text
revText t = T.pack (reverse (T.unpack t))

buttonClick :: (DomBuilder t m, PostBuild t m, MonadHold t m)
            => m (Event t T.Text)
buttonClick = do
   t <- inputElement def
   (e,_) <- el' "button" (text "OK")
   return $ attachPromptlyDynWith const
      (fmap revText (_inputElement_value t))
      (domEvent Click e)

bttnEvt :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m ()
bttnEvt = do
   evt <- buttonClick
   hl <- holdDyn "" evt -- Event -> Dynamic
   el "div" (dynText hl)
   
numberInput :: (DomBuilder t m, Num a, Read a) => m (Dynamic t a)
numberInput = do
   n <- inputElement $ def
      & inputElementConfig_initialValue .~ "0"
      & inputElementConfig_elementConfig
      . elementConfig_initialAttributes .~ ("type" =: "number")
   return $ fmap (fromMaybe 0 . readMaybe . T.unpack)
                 (_inputElement_value n)

sumButton :: (DomBuilder t m, PostBuild t m, MonadHold t m)
          => m (Event t Double)
sumButton = do
   n1 <- numberInput
   text " "
   n2 <- numberInput
   text " "
   (e,_) <- el' "button" (text "OK")
   let dynDouble = zipDynWith (+) n1 n2
   return $ attachPromptlyDynWith const
      dynDouble
      (domEvent Click e)

sumEvt :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m ()
sumEvt = do
   evt <- sumButton
   s <- holdDyn 0 evt
   el "div" (dynText $ fmap (T.pack . show) s)

caixas :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m ()
caixas = el "div" $ do
  t <- inputElement def -- Dynamic Text
  s <- inputElement def -- Dynamic Text
  text " "
  dynText $ zipDynWith (<>) (_inputElement_value t) (_inputElement_value s)

caixaSoma :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m ()
caixaSoma = do
   n1 <- numberInput -- m (Dynamic t Double)
   text " "
   n2 <- numberInput -- m (Dynamic t Double)
   dynText (fmap (T.pack . show) (zipDynWith (+) n1 n2))

grupo :: DomBuilder t m => m ()
grupo = do
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
      mainPag
      el "p" $ text $ T.pack commonStuff
      prerender_ blank $ liftJSM $ void $ eval ("console.log('Hello, World!')" :: T.Text)
      elAttr "img" ("src" =: static @"logo-fatec.png") blank
      el "div" $ do
        exampleConfig <- getConfig "common/example"
        case exampleConfig of
          Nothing -> text "No config file found in config/common/example"
          Just s -> text $ T.decodeUtf8 s
      return ()
  }
