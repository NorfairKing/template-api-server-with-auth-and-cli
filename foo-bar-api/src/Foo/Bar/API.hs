{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Foo.Bar.API where

import Data.Text (Text)
import Data.Validity.Text ()
import Foo.Bar.API.Data
import Servant.API
import Servant.API.Generic
import Servant.Auth

data FooBarRoutes route = FooBarRoutes
  { postRegister :: !(route :- PostRegister),
    postLogin :: !(route :- PostLogin),
    getGreet :: !(route :- GetGreet)
  }
  deriving (Generic)

type PostRegister =
  "register"
    :> ReqBody '[JSON] RegistrationForm
    :> PostNoContent

type PostLogin =
  "login"
    :> ReqBody '[JSON] LoginForm
    :> Post '[JSON] (Headers '[Header "Set-Cookie" Text] NoContent)

type ProtectAPI = Auth '[JWT] AuthCookie

type GetGreet =
  ProtectAPI
    :> "greet"
    :> Get '[JSON] Text
