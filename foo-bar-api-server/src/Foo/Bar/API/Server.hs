{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Foo.Bar.API.Server where

import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Text as T
import Database.Persist.Sql
import Database.Persist.Sqlite
import Foo.Bar.API as API
import Foo.Bar.API.Server.Env
import Foo.Bar.API.Server.Handler
import Foo.Bar.API.Server.OptParse
import Foo.Bar.API.Server.SigningKey
import Network.Wai as Wai
import Network.Wai.Handler.Warp as Warp
import Path
import Servant.Auth.Server
import Servant.Server.Generic

fooBarAPIServer :: IO ()
fooBarAPIServer = do
  Settings {..} <- getSettings
  runStderrLoggingT $
    withSqlitePool (T.pack (fromAbsFile settingDbFile)) 1 $ \pool -> do
      runSqlPool (runMigration serverMigration) pool
      liftIO $ do
        jwk <- loadSigningKey settingSigningKeyFile
        let serverEnv =
              Env
                { envConnectionPool = pool,
                  envCookieSettings = defaultCookieSettings,
                  envJWTSettings = defaultJWTSettings jwk
                }
        Warp.run settingPort $ fooBarAPIServerApp serverEnv

{-# ANN fooBarAPIServerApp ("NOCOVER" :: String) #-}
fooBarAPIServerApp :: Env -> Wai.Application
fooBarAPIServerApp env =
  genericServeTWithContext
    (flip runReaderT env)
    fooBarHandlers
    (fooBarContext env)

fooBarContext :: Env -> Context '[CookieSettings, JWTSettings]
fooBarContext Env {..} = envCookieSettings :. envJWTSettings :. EmptyContext

fooBarHandlers :: FooBarRoutes (AsServerT H)
fooBarHandlers =
  FooBarRoutes
    { postRegister = handlePostRegister,
      postLogin = handlePostLogin,
      getGreet = protected handleGetGreet
    }

protected :: ThrowAll m => (authCookie -> m) -> AuthResult authCookie -> m
protected func (Authenticated authCookie) = func authCookie
protected _ _ = throwAll err401
