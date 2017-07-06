{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.FreerClient where

import           GHC.TypeLits
import           Servant.API
import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.Http
import           Control.Monad.Freer.Reader
import           Data.ByteString.Lazy (ByteString)
import           Data.List
import           Data.Proxy
import           Data.Text (unpack)
import           Network.HTTP.Client (Response)
import           Network.HTTP.Media
import qualified Network.HTTP.Types         as H
import qualified Network.HTTP.Types.Header  as HTTP
import           Servant.Client hiding (ClientM, clientWithRoute)
import           Servant.Common.Req hiding (ClientM, runClientM', performRequest, performRequestCT, performRequestNoBody)
import           Servant.FreerReq

freeClient :: HasFreeClient r api => Proxy r -> Proxy api -> FreeClient r api
freeClient r p = clientWithRoute r p defReq

class HasFreeClient r api where
  type FreeClient (r :: [* -> *]) api :: *
  clientWithRoute :: Proxy r -> Proxy api -> Req -> FreeClient r api

instance (HasFreeClient r a, HasFreeClient r b) => HasFreeClient r (a :<|> b) where
  type FreeClient r (a :<|> b) = FreeClient r a :<|> FreeClient r b
  clientWithRoute r Proxy req =
    clientWithRoute r (Proxy :: Proxy a) req :<|>
    clientWithRoute r (Proxy :: Proxy b) req

instance HasFreeClient r EmptyAPI where
  type FreeClient r EmptyAPI = EmptyClient
  clientWithRoute Proxy Proxy _ = EmptyClient

instance (KnownSymbol capture, ToHttpApiData a, HasFreeClient r api)
      => HasFreeClient r (Capture capture a :> api) where
  type FreeClient r (Capture capture a :> api) =
    a -> FreeClient r api

  clientWithRoute r Proxy req val =
    clientWithRoute r (Proxy :: Proxy api) (appendToPath p req)
    where p = unpack (toUrlPiece val)

instance (KnownSymbol capture, ToHttpApiData a, HasFreeClient r sublayout)
      => HasFreeClient r (CaptureAll capture a :> sublayout) where

  type FreeClient r (CaptureAll capture a :> sublayout) =
    [a] -> FreeClient r sublayout

  clientWithRoute r Proxy req vals =
    clientWithRoute r (Proxy :: Proxy sublayout) (foldl' (flip appendToPath) req ps)
    where ps = map (unpack . toUrlPiece) vals

instance ( MimeUnrender ct a
         , ReflectMethod method
         , cts' ~ (ct ': cts)
         , Member Http r
         , Member (Reader ClientEnv) r
         , Member (Exc ServantError) r) => HasFreeClient r (Verb method status cts' a) where
  type FreeClient r (Verb method status cts' a) = ClientM r a
  clientWithRoute Proxy Proxy req = do
    snd <$> performRequestCT (Proxy :: Proxy ct) method req
      where method = reflectMethod (Proxy :: Proxy method)

instance ( ReflectMethod method
         , Member Http r
         , Member (Reader ClientEnv) r
         , Member (Exc ServantError) r) => HasFreeClient r (Verb method status cts NoContent) where
  type FreeClient r (Verb method status cts NoContent) =
    ClientM r NoContent
  clientWithRoute Proxy Proxy req = do
    performRequestNoBody method req >> return NoContent
      where method = reflectMethod (Proxy :: Proxy method)

instance ( MimeUnrender ct a
         , BuildHeadersTo ls
         , cts' ~ (ct ': cts)
         , ReflectMethod method
         , Member Http r
         , Member (Reader ClientEnv) r
         , Member (Exc ServantError) r
         ) => HasFreeClient r (Verb method status cts' (Headers ls a)) where
  type FreeClient r (Verb method status cts' (Headers ls a))
    = ClientM r (Headers ls a)
  clientWithRoute Proxy Proxy req = do
    let method = reflectMethod (Proxy :: Proxy method)
    (hdrs, resp) <- performRequestCT (Proxy :: Proxy ct) method req
    return $ Headers { getResponse = resp
                     , getHeadersHList = buildHeadersTo hdrs
                     }

instance ( BuildHeadersTo ls
         , ReflectMethod method
         , Member Http r
         , Member (Reader ClientEnv) r
         , Member (Exc ServantError) r
         ) => HasFreeClient r (Verb method status cts (Headers ls NoContent)) where
  type FreeClient r (Verb method status cts (Headers ls NoContent))
    = ClientM r (Headers ls NoContent)
  clientWithRoute Proxy Proxy req = do
    let method = reflectMethod (Proxy :: Proxy method)
    hdrs <- performRequestNoBody method req
    return $ Headers { getResponse = NoContent
                     , getHeadersHList = buildHeadersTo hdrs
                     }

instance ( Member Http r
         , Member (Reader ClientEnv) r
         , Member (Exc ServantError) r) => HasFreeClient r Raw where
  type FreeClient r Raw
    = H.Method -> ClientM r ( Int
                            , ByteString
                            , MediaType
                            , [HTTP.Header]
                            , Response ByteString)

  clientWithRoute :: Proxy r -> Proxy Raw -> Req -> FreeClient r Raw
  clientWithRoute Proxy Proxy req httpMethod = do
    performRequest httpMethod req
