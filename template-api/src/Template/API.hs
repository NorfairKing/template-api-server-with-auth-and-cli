{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Template.API where

import Crypto.JWT
import Data.Aeson
import Data.Proxy
import qualified Data.Text as T
import Data.Text (Text)
import Data.Validity
import Data.Validity.Text ()
import Database.Persist
import Database.Persist.Sql
import GHC.Generics (Generic)
import Servant.API
import Servant.API.Generic
import Servant.Auth
import Servant.Auth.Server
import Template.Data

templateAPI :: Proxy TemplateAPI
templateAPI = Proxy

type TemplateAPI = ToServantApi TemplateRoutes

data TemplateRoutes route
  = TemplateRoutes
      { postRegister :: !(route :- PostRegister),
        postLogin :: !(route :- PostLogin),
        getGreeting :: !(route :- GetGreeting)
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

type GetGreeting =
  ProtectAPI
    :> "greet"
    :> QueryParam "greeting" Text
    :> Get '[JSON] Text
