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

getLivrReq :: Int -> XhrRequest ()
getLivrReq pid = xhrRequest "GET" (getPath (BackendRoute_BuscarLivr :/ pid)) def

getFuncReq :: Int -> XhrRequest ()
getFuncReq pid = xhrRequest "GET" (getPath (BackendRoute_BuscarFunc :/ pid)) def

getCompReq :: Int -> XhrRequest ()
getCompReq pid = xhrRequest "GET" (getPath (BackendRoute_BuscarComp :/ pid)) def

getDelLivrReq :: Int -> XhrRequest ()
getDelLivrReq pid = xhrRequest "DELETE" (getPath (BackendRoute_DeletarLivr :/ pid)) def

getDelFuncReq :: Int -> XhrRequest ()
getDelFuncReq pid = xhrRequest "DELETE" (getPath (BackendRoute_DeletarFunc :/ pid)) def

getDelCompReq :: Int -> XhrRequest ()
getDelCompReq pid = xhrRequest "DELETE" (getPath (BackendRoute_DeletarComp :/ pid)) def

getListLivrReq :: XhrRequest ()
getListLivrReq = xhrRequest "GET" (getPath (BackendRoute_ListarLivr :/ ())) def

getListFuncReq :: XhrRequest ()
getListFuncReq = xhrRequest "GET" (getPath (BackendRoute_ListarFunc :/ ())) def

getListCompReq :: XhrRequest ()
getListCompReq = xhrRequest "GET" (getPath (BackendRoute_ListarComp :/ ())) def

sendRequest :: ToJSON a => R BackendRoute -> a -> XhrRequest T.Text
sendRequest r dados = postJson (getPath r) dados
 
reqLivr :: ( DomBuilder t m
       , Prerender js t m
       ) => m ()
reqLivr = do
   el "div" (text "Título: ")
   titulo <- inputElement def
   el "div" (text "Autor: ")
   autor <- inputElement def
   el "div" (text "Editora: ")
   editora <- inputElement def
   el "div" (text "Categoria: ")
   categoria <- inputElement def
   el "div" (text "Valor: ")
   valor <- numberInput
   el "div" (text "Quantidade: ")
   qt <- numberInput
   el "div" (text "")
   let livr = fmap (\(((((t,a),e),c),v),q) -> Livro 0 t a e c v q) (zipDyn (zipDyn (zipDyn (zipDyn (zipDyn (_inputElement_value titulo) (_inputElement_value autor)) (_inputElement_value editora)) (_inputElement_value categoria)) valor) qt)
   (submitBtn,_) <- el' "button" (text "Inserir")
   let click = domEvent Click submitBtn
   let livrEvt = tag (current livr) click
   _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_Livro :/ ()) <$> livrEvt))
   return ()

reqFunc :: ( DomBuilder t m
       , Prerender js t m
       ) => m ()
reqFunc = do
   el "div" (text "Nome: ")
   nome <- inputElement def
   el "div" (text "Cargo: ")
   cargo <- inputElement def
   el "div" (text "e-mail: ")
   email <- inputElement def
   el "div" (text "Telefone: ")
   telefone <- inputElement def
   el "div" (text "Salário: ")
   salario <- numberInput
   el "div" (text "")
   let func = fmap (\((((n,c),e),t),s) -> Funcionario 0 n c e t s) (zipDyn (zipDyn (zipDyn (zipDyn (_inputElement_value nome) (_inputElement_value cargo)) (_inputElement_value email)) (_inputElement_value telefone)) salario)
   (submitBtn,_) <- el' "button" (text "Inserir")
   let click = domEvent Click submitBtn
   let funcEvt = tag (current func) click
   _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_Funcionario :/ ()) <$> funcEvt))
   return ()
   
reqComp :: ( DomBuilder t m
       , Prerender js t m
       ) => m ()
reqComp = do
   el "div" (text "Nome: ")
   nome <- inputElement def
   el "div" (text "Endereço: ")
   endereco <- inputElement def
   el "div" (text "Cidade: ")
   cidade <- inputElement def
   el "div" (text "Estado: ")
   estado <- inputElement def
   el "div" (text "e-mail: ")
   email <- inputElement def
   el "div" (text "")
   let comp = fmap (\((((a,b),c),d),e) -> Comprador 0 a b c d e) (zipDyn (zipDyn (zipDyn (zipDyn (_inputElement_value nome) (_inputElement_value endereco)) (_inputElement_value cidade)) (_inputElement_value estado)) (_inputElement_value email))
   (submitBtn,_) <- el' "button" (text "Inserir")
   let click = domEvent Click submitBtn
   let compEvt = tag (current comp) click
   _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_Comprador :/ ()) <$> compEvt))
   return ()

data Acao = Perfil Int | Editar Int | Deletar Int
     
tabLivro :: (PostBuild t m, DomBuilder t m) => Dynamic t Livro
                                              -> m (Event t Acao)
tabLivro pr = do
   el "tr" $ do
      el "td" (dynText $ fmap (T.pack . show . livroId) pr)
      el "td" (dynText $ fmap (T.pack . show . livroTitulo) pr)
      el "td" (dynText $ fmap (T.pack . show . livroAutor) pr)
      el "td" (dynText $ fmap (T.pack . show . livroEditora) pr)
      el "td" (dynText $ fmap (T.pack . show . livroCategoria) pr)
      el "td" (dynText $ fmap (T.pack . show . livroValor) pr)
      el "td" (dynText $ fmap (T.pack . show . livroQt) pr)
      evt1 <- fmap (fmap (const Perfil)) (button "Perfil")
      evt2 <- fmap (fmap (const Editar)) (button "Editar")
      evt3 <- fmap (fmap (const Deletar)) (button "Deletar")
      return (attachPromptlyDynWith (flip ($)) (fmap livroId pr) (leftmost [evt1,evt2,evt3]))
      
tabFuncionario :: (PostBuild t m, DomBuilder t m) => Dynamic t Funcionario
                                              -> m (Event t Acao)
tabFuncionario pr = do
   el "tr" $ do
      el "td" (dynText $ fmap (T.pack . show . funcionarioId) pr)
      el "td" (dynText $ fmap (T.pack . show . funcionarioNome) pr)
      el "td" (dynText $ fmap (T.pack . show . funcionarioCargo) pr)
      el "td" (dynText $ fmap (T.pack . show . funcionarioEmail) pr)
      el "td" (dynText $ fmap (T.pack . show . funcionarioTelefone) pr)
      el "td" (dynText $ fmap (T.pack . show . funcionarioSalario) pr)
      evt1 <- fmap (fmap (const Perfil)) (button "Perfil")
      evt2 <- fmap (fmap (const Editar)) (button "Editar")
      evt3 <- fmap (fmap (const Deletar)) (button "Deletar")
      return (attachPromptlyDynWith (flip ($)) (fmap funcionarioId pr) (leftmost [evt1,evt2,evt3]))
      
tabComprador :: (PostBuild t m, DomBuilder t m) => Dynamic t Comprador
                                              -> m (Event t Acao)
tabComprador pr = do
   el "tr" $ do
      el "td" (dynText $ fmap (T.pack . show . compradorId) pr)
      el "td" (dynText $ fmap (T.pack . show . compradorNome) pr)
      el "td" (dynText $ fmap (T.pack . show . compradorEndereco) pr)
      el "td" (dynText $ fmap (T.pack . show . compradorCidade) pr)
      el "td" (dynText $ fmap (T.pack . show . compradorEstado) pr)
      el "td" (dynText $ fmap (T.pack . show . compradorEmail) pr)
      evt1 <- fmap (fmap (const Perfil)) (button "Perfil")
      evt2 <- fmap (fmap (const Editar)) (button "Editar")
      evt3 <- fmap (fmap (const Deletar)) (button "Deletar")
      return (attachPromptlyDynWith (flip ($)) (fmap compradorId pr) (leftmost [evt1,evt2,evt3]))

editarLivro :: ( DomBuilder t m
             , Prerender js t m
             , MonadHold t m
             , MonadFix m
             , PostBuild t m) => Int -> Workflow t m T.Text
editarLivro pid = Workflow $ do
   btn <- button "Mostrar"
   livr :: Dynamic t (Event t (Maybe Livro)) <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (const (getLivrReq pid) <$> btn))
   mdyn <- return (switchDyn livr)
   dynE <- return ((fromMaybe (Livro 0 "" "" "" "" 0 0)) <$> mdyn)
   el "div" (text "Título: ")
   titulo <- inputElement $ def & inputElementConfig_setValue .~ (fmap livroTitulo dynE)
   el "div" (text "Autor: ")
   autor <- inputElement $ def & inputElementConfig_setValue .~ (fmap livroAutor dynE)
   el "div" (text "Editora: ")
   editora <- inputElement $ def & inputElementConfig_setValue .~ (fmap livroEditora dynE)
   el "div" (text "Categoria: ")
   categoria <- inputElement $ def & inputElementConfig_setValue .~ (fmap livroCategoria dynE)
   el "div" (text "Valor: ")
   valor <- numberInputDyn (fmap livroValor dynE)
   el "div" (text "Quantidade: ")
   qt <- numberInputDyn (fmap livroQt dynE)
   el "div" (text "")
   let livr = fmap (\(((((t,a),e),c),v),q) -> Livro 0 t a e c v q) (zipDyn (zipDyn (zipDyn (zipDyn (zipDyn (_inputElement_value titulo) (_inputElement_value autor)) (_inputElement_value editora)) (_inputElement_value categoria)) valor) qt)
   submitBtn <- button "Editar"
   let livrEvt = tag (current livr) submitBtn
   _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_EditarLivr :/ pid) <$> livrEvt))
   return ("Id Livro: " <> (T.pack $ show pid), reqTabelaLivr <$ submitBtn)
   where
      novoInput x = inputElement $ def
         & inputElementConfig_elementConfig
         . elementConfig_initialAttributes .~ ("value" =: x)

editarFuncionario :: ( DomBuilder t m
             , Prerender js t m
             , MonadHold t m
             , MonadFix m
             , PostBuild t m) => Int -> Workflow t m T.Text
editarFuncionario pid = Workflow $ do
   btn <- button "Mostrar"
   func :: Dynamic t (Event t (Maybe Funcionario)) <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (const (getFuncReq pid) <$> btn))
   mdyn <- return (switchDyn func)
   dynE <- return ((fromMaybe (Funcionario 0 "" "" "" "" 0)) <$> mdyn)
   el "div" (text "Nome: ")
   nome <- inputElement $ def & inputElementConfig_setValue .~ (fmap funcionarioNome dynE)
   el "div" (text "Cargo: ")
   cargo <- inputElement $ def & inputElementConfig_setValue .~ (fmap funcionarioCargo dynE)
   el "div" (text "e-mail: ")
   email <- inputElement $ def & inputElementConfig_setValue .~ (fmap funcionarioEmail dynE)
   el "div" (text "Telefone: ")
   telefone <- inputElement $ def & inputElementConfig_setValue .~ (fmap funcionarioTelefone dynE)
   el "div" (text "Salário: ")
   salario <- numberInputDyn (fmap funcionarioSalario dynE)
   el "div" (text "")
   let func = fmap (\((((n,c),e),t),s) -> Funcionario 0 n c e t s) (zipDyn (zipDyn (zipDyn (zipDyn (_inputElement_value nome) (_inputElement_value cargo)) (_inputElement_value email)) (_inputElement_value telefone)) salario)
   submitBtn <- button "Editar"
   let funcEvt = tag (current func) submitBtn
   _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_EditarFunc :/ pid) <$> funcEvt))
   return ("Id Funcionário: " <> (T.pack $ show pid), reqTabelaFunc <$ submitBtn)
   where
      novoInput x = inputElement $ def
         & inputElementConfig_elementConfig
         . elementConfig_initialAttributes .~ ("value" =: x)

editarComprador :: ( DomBuilder t m
             , Prerender js t m
             , MonadHold t m
             , MonadFix m
             , PostBuild t m) => Int -> Workflow t m T.Text
editarComprador pid = Workflow $ do
   btn <- button "Mostrar"
   comp :: Dynamic t (Event t (Maybe Comprador)) <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (const (getCompReq pid) <$> btn))
   mdyn <- return (switchDyn comp)
   dynE <- return ((fromMaybe (Comprador 0 "" "" "" "" "")) <$> mdyn)
   el "div" (text "Nome: ")
   nome <- inputElement $ def & inputElementConfig_setValue .~ (fmap compradorNome dynE)
   el "div" (text "Endereço: ")
   endereco <- inputElement $ def & inputElementConfig_setValue .~ (fmap compradorEndereco dynE)
   el "div" (text "Cidade: ")
   cidade <- inputElement $ def & inputElementConfig_setValue .~ (fmap compradorCidade dynE)
   el "div" (text "Estado: ")
   estado <- inputElement $ def & inputElementConfig_setValue .~ (fmap compradorEstado dynE)
   el "div" (text "e-mail: ")
   email <- inputElement $ def & inputElementConfig_setValue .~ (fmap compradorEmail dynE)
   el "div" (text "")
   let comp = fmap (\((((a,b),c),d),e) -> Comprador 0 a b c d e) (zipDyn (zipDyn (zipDyn (zipDyn (_inputElement_value nome) (_inputElement_value endereco)) (_inputElement_value cidade)) (_inputElement_value estado)) (_inputElement_value email))
   submitBtn <- button "Editar"
   let compEvt = tag (current comp) submitBtn
   _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_EditarComp :/ pid) <$> compEvt))
   return ("Id Cliente: " <> (T.pack $ show pid), reqTabelaComp <$ submitBtn)
   where
      novoInput x = inputElement $ def
         & inputElementConfig_elementConfig
         . elementConfig_initialAttributes .~ ("value" =: x)

deletarLivro :: ( DomBuilder t m
              , Prerender js t m
              , MonadHold t m
              , MonadFix m
              , PostBuild t m) => Int -> Workflow t m T.Text
deletarLivro pid = Workflow $ do
   btn <- button "Deletar"
   livr :: Dynamic t (Event t (Maybe Livro)) <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (const (getDelLivrReq pid) <$> btn))
   mdyn <- holdDyn Nothing (switchDyn livr)
   dynP <- return ((fromMaybe (Livro 0 "" "" "" "" 0 0)) <$> mdyn)
   ret <- button "Voltar"
   return ("Id Livro: " <> (T.pack $ show pid), reqTabelaLivr <$ ret)
   
deletarFuncionario :: ( DomBuilder t m
              , Prerender js t m
              , MonadHold t m
              , MonadFix m
              , PostBuild t m) => Int -> Workflow t m T.Text
deletarFuncionario pid = Workflow $ do
   btn <- button "Deletar"
   func :: Dynamic t (Event t (Maybe Funcionario)) <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (const (getDelFuncReq pid) <$> btn))
   ret <- button "Voltar"
   return ("Id Funcionário: " <> (T.pack $ show pid), reqTabelaFunc <$ ret)
   
deletarComprador :: ( DomBuilder t m
              , Prerender js t m
              , MonadHold t m
              , MonadFix m
              , PostBuild t m) => Int -> Workflow t m T.Text
deletarComprador pid = Workflow $ do
   btn <- button "Deletar"
   comp :: Dynamic t (Event t (Maybe Comprador)) <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (const (getDelCompReq pid) <$> btn))
   ret <- button "Voltar"
   return ("Id Comprador: " <> (T.pack $ show pid), reqTabelaComp <$ ret)

pagLivro :: ( DomBuilder t m
             , Prerender js t m
             , MonadHold t m
             , MonadFix m
             , PostBuild t m) => Int -> Workflow t m T.Text
pagLivro pid = Workflow $ do
   btn <- button "Mostrar"
   livr :: Dynamic t (Event t (Maybe Livro)) <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (const (getLivrReq pid) <$> btn))
   mdyn <- holdDyn Nothing (switchDyn livr)
   dynP <- return ((fromMaybe (Livro 0 "" "" "" "" 0 0)) <$> mdyn)
   el "div" $ do
      el "div" (text "Título: ")
      el "div" (dynText $ fmap livroTitulo dynP)
      el "div" (text "Autor: ")
      el "div" (dynText $ fmap livroAutor dynP)
      el "div" (text "Editora: ")
      el "div" (dynText $ fmap livroEditora dynP)
      el "div" (text "Categoria: ")
      el "div" (dynText $ fmap livroCategoria dynP)
      el "div" (text "Valor: ")
      el "div" (dynText $ fmap (T.pack . show . livroValor) dynP)
      el "div" (text "Quantidade: ")
      el "div" (dynText $ fmap (T.pack . show . livroQt) dynP)
   ret <- button "Voltar"
   return ("Id Livro: " <> (T.pack $ show pid), reqTabelaLivr <$ ret)

pagFuncionario :: ( DomBuilder t m
             , Prerender js t m
             , MonadHold t m
             , MonadFix m
             , PostBuild t m) => Int -> Workflow t m T.Text
pagFuncionario pid = Workflow $ do
   btn <- button "Mostrar"
   func :: Dynamic t (Event t (Maybe Funcionario)) <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (const (getFuncReq pid) <$> btn))
   mdyn <- holdDyn Nothing (switchDyn func)
   dynP <- return ((fromMaybe (Funcionario 0 "" "" "" "" 0)) <$> mdyn)
   el "div" $ do
      el "div" (text "Nome: ")
      el "div" (dynText $ fmap funcionarioNome dynP)
      el "div" (text "Cargo: ")
      el "div" (dynText $ fmap funcionarioCargo dynP)
      el "div" (text "e-mail: ")
      el "div" (dynText $ fmap funcionarioEmail dynP)
      el "div" (text "Telefone: ")
      el "div" (dynText $ fmap funcionarioTelefone dynP)
      el "div" (text "Salário: ")
      el "div" (dynText $ fmap (T.pack . show . funcionarioSalario) dynP)
   ret <- button "Voltar"
   return ("Id Funcionário: " <> (T.pack $ show pid), reqTabelaFunc <$ ret)

pagComprador :: ( DomBuilder t m
             , Prerender js t m
             , MonadHold t m
             , MonadFix m
             , PostBuild t m) => Int -> Workflow t m T.Text
pagComprador pid = Workflow $ do
   btn <- button "Mostrar"
   comp :: Dynamic t (Event t (Maybe Comprador)) <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (const (getCompReq pid) <$> btn))
   mdyn <- holdDyn Nothing (switchDyn comp)
   dynP <- return ((fromMaybe (Comprador 0 "" "" "" "" "")) <$> mdyn)
   el "div" $ do
      el "div" (text "Nome: ")
      el "div" (dynText $ fmap compradorNome dynP)
      el "div" (text "Endereço: ")
      el "div" (dynText $ fmap compradorEndereco dynP)
      el "div" (text "Cidade: ")
      el "div" (dynText $ fmap compradorCidade dynP)
      el "div" (text "Estado: ")
      el "div" (dynText $ fmap compradorEstado dynP)
      el "div" (text "e-mail: ")
      el "div" (dynText $ fmap compradorEmail dynP)
   ret <- button "Voltar"
   return ("Id Comprador: " <> (T.pack $ show pid), reqTabelaComp <$ ret)
    
reqTabelaLivr :: ( DomBuilder t m
              , Prerender js t m
              , MonadHold t m
              , MonadFix m
              , PostBuild t m) => Workflow t m T.Text
reqTabelaLivr = Workflow $ do
   btn <- button "Listar"
   prods :: Dynamic t (Event t (Maybe [Livro])) <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (const getListLivrReq <$> btn))
   evt <- return (fmap (fromMaybe []) $ switchDyn prods)
   dynP <- foldDyn (++) [] evt
   tb <- el "table" $ do
      el "thead" $ do
         el "tr" $ do
            el "th" (text "Id")
            el "th" (text "Título")
            el "th" (text "Autor")
            el "th" (text "Editora")
            el "th" (text "Categoria")
            el "th" (text "Valor")
            el "th" (text "Quantidade")
            el "th" blank
            el "th" blank
            el "th" blank
      el "tbody" $ do
         simpleList dynP tabLivro
   tb' <- return $ switchDyn $ fmap leftmost tb
   return ("Lista de livros", escolherPag <$> tb')
   where
      escolherPag (Perfil pid) = pagLivro pid
      escolherPag (Editar pid) = editarLivro pid
      escolherPag (Deletar pid) = deletarLivro pid

reqTabelaFunc :: ( DomBuilder t m
              , Prerender js t m
              , MonadHold t m
              , MonadFix m
              , PostBuild t m) => Workflow t m T.Text
reqTabelaFunc = Workflow $ do
   btn <- button "Listar"
   prods :: Dynamic t (Event t (Maybe [Funcionario])) <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (const getListFuncReq <$> btn))
   evt <- return (fmap (fromMaybe []) $ switchDyn prods)
   dynP <- foldDyn (++) [] evt
   tb <- el "table" $ do
      el "thead" $ do
         el "tr" $ do
            el "th" (text "Id")
            el "th" (text "Nome")
            el "th" (text "Cargo")
            el "th" (text "e-mail")
            el "th" (text "Telefone")
            el "th" (text "Salário")
            el "th" blank
            el "th" blank
            el "th" blank
      el "tbody" $ do
         simpleList dynP tabFuncionario
   tb' <- return $ switchDyn $ fmap leftmost tb
   return ("Lista de funcionários", escolherPag <$> tb')
   where
      escolherPag (Perfil pid) = pagFuncionario pid
      escolherPag (Editar pid) = editarFuncionario pid
      escolherPag (Deletar pid) = deletarFuncionario pid

reqTabelaComp :: ( DomBuilder t m
              , Prerender js t m
              , MonadHold t m
              , MonadFix m
              , PostBuild t m) => Workflow t m T.Text
reqTabelaComp = Workflow $ do
   btn <- button "Listar"
   prods :: Dynamic t (Event t (Maybe [Comprador])) <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (const getListCompReq <$> btn))
   evt <- return (fmap (fromMaybe []) $ switchDyn prods)
   dynP <- foldDyn (++) [] evt
   tb <- el "table" $ do
      el "thead" $ do
         el "tr" $ do
            el "th" (text "Id")
            el "th" (text "Nome")
            el "th" (text "Endereço")
            el "th" (text "Cidade")
            el "th" (text "Estado")
            el "th" (text "e-mail")
            el "th" blank
            el "th" blank
            el "th" blank
      el "tbody" $ do
         simpleList dynP tabComprador
   tb' <- return $ switchDyn $ fmap leftmost tb
   return ("Lista de clientes", escolherPag <$> tb')
   where
      escolherPag (Perfil pid) = pagComprador pid
      escolherPag (Editar pid) = editarComprador pid
      escolherPag (Deletar pid) = deletarComprador pid

reqListaLivr :: ( DomBuilder t m
             , Prerender js t m
             , MonadHold t m
             , MonadFix m
             , PostBuild t m) => m ()
reqListaLivr = do
   r <- workflow reqTabelaLivr
   el "div" (dynText r)

reqListaFunc :: ( DomBuilder t m
             , Prerender js t m
             , MonadHold t m
             , MonadFix m
             , PostBuild t m) => m ()
reqListaFunc = do
   r <- workflow reqTabelaFunc
   el "div" (dynText r)

reqListaComp :: ( DomBuilder t m
             , Prerender js t m
             , MonadHold t m
             , MonadFix m
             , PostBuild t m) => m ()
reqListaComp = do
   r <- workflow reqTabelaComp
   el "div" (dynText r)

clickLi :: (DomBuilder t m, PostBuild t m, MonadHold t m) => Pagina -> T.Text -> m (Event t Pagina)
clickLi p t = do
   (ev, _) <- el' "li" (elAttr "a" ("href" =: "#") (text t))
   return ((\_ -> p) <$> domEvent Click ev)

menuLi :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m (Dynamic t Pagina)
menuLi = do
   evs <- el "ul" $ do
      p1 <- clickLi Pagina1 "Inserção de livro"
      p2 <- clickLi Pagina2 "Lista de livros"
      p3 <- clickLi Pagina3 "Inserção de funcionário"
      p4 <- clickLi Pagina4 "Lista de funcionários"
      p5 <- clickLi Pagina5 "Inserção de cliente"
      p6 <- clickLi Pagina6 "Lista de clientes"
      return (leftmost [p1,p2,p3,p4,p5,p6])
   holdDyn Pagina0 evs

currPag :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m, Prerender js t m)
        => Pagina -> m ()
currPag p =
   case p of
      Pagina0 -> blank
      Pagina1 -> reqLivr
      Pagina2 -> reqListaLivr
      Pagina3 -> reqFunc
      Pagina4 -> reqListaFunc
      Pagina5 -> reqComp
      Pagina6 -> reqListaComp

mainPag :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m, Prerender js t m) => m ()
mainPag = do
   pag <- el "div" menuLi
   dyn_ $ currPag <$> pag
   
numberInput :: (DomBuilder t m, Num a, Read a) => m (Dynamic t a)
numberInput = do
   n <- inputElement $ def
      & inputElementConfig_initialValue .~ "0"
      & inputElementConfig_elementConfig
      . elementConfig_initialAttributes .~ ("type" =: "number")
   return $ fmap (fromMaybe 0 . readMaybe . T.unpack)
                 (_inputElement_value n)
                 
numberInputDyn :: (DomBuilder t m, Num a, Read a, Show a) => Event t a -> m (Dynamic t a)
numberInputDyn p = do
   val <- return (fmap (T.pack . show) p)
   n <- inputElement $ def
     & inputElementConfig_setValue .~ val
   return $ fmap (fromMaybe 0 . readMaybe . T.unpack)
                 (_inputElement_value n)

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Livraria Brasileira"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      el "h1" $ text "Livraria Brasileira"
      mainPag
      prerender_ blank $ liftJSM $ void $ eval ("console.log('Hello, World!')" :: T.Text)
      el "h3" $ text "Integrantes: Juliana de Oliveira Angotti | Pedro Silva de Sá Monnerat | Renata Fuschini Alaggio"
      elAttr "img" ("src" =: static @"bookstack.jpg" <> "height" =: "300" <> "width" =: "300") blank
      return ()
  }
