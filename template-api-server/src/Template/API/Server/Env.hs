{-# LANGUAGE RecordWildCards #-}

module Template.API.Server.Env where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Database.Persist
import Database.Persist.Sql
import Network.Wai as Wai
import Servant
import Servant.Auth.Server
import Servant.Server
import Servant.Server.Generic
import Template.API

type H = ReaderT Env Handler

data Env
  = Env
      { envConnectionPool :: ConnectionPool,
        envCookieSettings :: CookieSettings,
        envJWTSettings :: JWTSettings
      }

runDB :: SqlPersistT IO a -> H a
runDB func = do
  pool <- asks envConnectionPool
  liftIO $ runSqlPool func pool
