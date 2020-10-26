{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Foobar.API.Server where

import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Text as T
import Database.Persist.Sql
import Database.Persist.Sqlite
import Foobar.API as API
import Foobar.API.Server.Env
import Foobar.API.Server.Handler
import Foobar.API.Server.OptParse
import Foobar.API.Server.SigningKey
import Network.Wai as Wai
import Network.Wai.Handler.Warp as Warp
import Path
import Servant.Auth.Server
import Servant.Server.Generic

foobarAPIServer :: IO ()
foobarAPIServer = do
  Settings {..} <- getSettings
  runStderrLoggingT $ withSqlitePool (T.pack (fromAbsFile settingDbFile)) 1 $ \pool -> do
    runSqlPool (runMigration serverMigration) pool
    liftIO $ do
      jwk <- loadSigningKey settingSigningKeyFile
      let serverEnv =
            Env
              { envConnectionPool = pool,
                envCookieSettings = defaultCookieSettings,
                envJWTSettings = defaultJWTSettings jwk
              }
      Warp.run settingPort $ foobarAPIServerApp serverEnv

foobarAPIServerApp :: Env -> Wai.Application
foobarAPIServerApp env =
  genericServeTWithContext
    (flip runReaderT env)
    foobarHandlers
    (foobarContext env)

foobarContext :: Env -> Context '[CookieSettings, JWTSettings]
foobarContext Env {..} = envCookieSettings :. envJWTSettings :. EmptyContext

foobarHandlers :: FoobarRoutes (AsServerT H)
foobarHandlers =
  FoobarRoutes
    { postRegister = handlePostRegister,
      postLogin = handlePostLogin,
      getGreet = protected handleGetGreet
    }

protected :: ThrowAll m => (authCookie -> m) -> AuthResult authCookie -> m
protected func (Authenticated authCookie) = func authCookie
protected _ _ = throwAll err401
