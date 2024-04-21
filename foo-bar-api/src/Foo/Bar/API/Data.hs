{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Foo.Bar.API.Data where

import Autodocodec
import Control.Arrow (left)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text ()
import Database.Persist
import Database.Persist.Sql
import Servant.API.Generic
import Servant.Auth.Server

data RegistrationForm = RegistrationForm
  { registrationFormUsername :: Username,
    registrationFormPassword :: Text
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec RegistrationForm)

instance Validity RegistrationForm where
  validate rf@RegistrationForm {..} =
    mconcat
      [ genericValidate rf,
        declare "The password is nonempty" $ not $ T.null registrationFormPassword
      ]

instance HasCodec RegistrationForm where
  codec =
    object "RegistrationForm" $
      RegistrationForm
        <$> requiredField "username" "username"
          .= registrationFormUsername
        <*> requiredField "password" "password"
          .= registrationFormPassword

data LoginForm = LoginForm
  { loginFormUsername :: Username,
    loginFormPassword :: Text
  }
  deriving (Show, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec LoginForm)

instance Validity LoginForm

instance HasCodec LoginForm where
  codec =
    object "LoginForm" $
      LoginForm
        <$> requiredField "username" "username"
          .= loginFormUsername
        <*> requiredField "password" "password"
          .= loginFormPassword

data AuthCookie = AuthCookie
  { authCookieUsername :: Username
  }
  deriving (Generic)

instance FromJSON AuthCookie

instance ToJSON AuthCookie

instance FromJWT AuthCookie

instance ToJWT AuthCookie

newtype Username = Username
  { usernameText :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (PersistFieldSql)
  deriving (FromJSON, ToJSON) via (Autodocodec Username)

instance Validity Username where
  validate (Username t) =
    mconcat
      [ check (not (T.null t)) "The username is not empty.",
        check (T.length t >= 3) "The username is at least three characters long."
      ]

instance HasCodec Username where
  codec = bimapCodec parseUsernameOrErr usernameText codec

instance PersistField Username where
  fromPersistValue pv = do
    t <- fromPersistValue pv
    left T.pack $ parseUsernameOrErr t
  toPersistValue = toPersistValue . usernameText

parseUsernameOrErr :: Text -> Either String Username
parseUsernameOrErr = prettyValidate . Username
