{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Foobar.API where

import Data.Proxy
import Data.Text (Text)
import Data.Validity.Text ()
import Foobar.API.Data
import Servant.API
import Servant.API.Generic
import Servant.Auth

foobarAPI :: Proxy FoobarAPI
foobarAPI = Proxy

type FoobarAPI = ToServantApi FoobarRoutes

data FoobarRoutes route
  = FoobarRoutes
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
