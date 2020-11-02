{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Foo.Bar.API where

import Data.Proxy
import Data.Text (Text)
import Data.Validity.Text ()
import Foo.Bar.API.Data
import Servant.API
import Servant.API.Generic
import Servant.Auth

fooBarAPI :: Proxy FooBarAPI
fooBarAPI = Proxy

type FooBarAPI = ToServantApi FooBarRoutes

data FooBarRoutes route
  = FooBarRoutes
      { postRegister :: !(route :- PostRegister),
        postLogin :: !(route :- PostLogin),
        getGreet :: !(route :- GetGreet)
      }
  deriving (Generic)

type PostRegister =
  "register"
    :> ReqBody '[JSON] RegistrationForm
    :> Post '[JSON] NoContent

type PostLogin =
  "login"
    :> ReqBody '[JSON] LoginForm
    :> PostNoContent '[JSON] (Headers '[Header "Set-Cookie" Text] NoContent)

type ProtectAPI = Auth '[JWT] AuthCookie

type GetGreet =
  ProtectAPI
    :> "greet"
    :> Get '[JSON] Text
