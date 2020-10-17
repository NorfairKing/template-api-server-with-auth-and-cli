{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Template.API.Server where

import Control.Monad.Logger
import Control.Monad.Reader
import Database.Persist.Sql
import Database.Persist.Sqlite
import Network.Wai as Wai
import Network.Wai.Handler.Warp as Warp
import Servant
import Servant.Auth.Server
import Servant.Server.Generic
import Template.API as API
import Template.API.Server.Env
import Template.API.Server.Handler

templateAPIServer :: IO ()
templateAPIServer = do
  runStderrLoggingT $ withSqlitePool "template.sqlite3" 1 $ \pool -> do
    runSqlPool (runMigration migrateAll) pool
    liftIO $ do
      jwk <- generateKey
      let serverEnv =
            Env
              { envConnectionPool = pool,
                envCookieSettings = defaultCookieSettings,
                envJWTSettings = defaultJWTSettings jwk
              }
      Warp.run 8000 $ templateAPIServerApp serverEnv

templateAPIServerApp :: Env -> Wai.Application
templateAPIServerApp env =
  genericServeTWithContext
    (flip runReaderT env)
    templateHandlers
    (templateContext env)

templateContext :: Env -> Context '[CookieSettings, JWTSettings]
templateContext Env {..} = envCookieSettings :. envJWTSettings :. EmptyContext

templateHandlers :: TemplateRoutes (AsServerT H)
templateHandlers =
  TemplateRoutes
    { postRegister = handlePostRegister,
      postLogin = handlePostLogin,
      getGreeting = protected handleGetGreeting
    }

protected :: ThrowAll m => (authCookie -> m) -> AuthResult authCookie -> m
protected func (Authenticated authCookie) = func authCookie
protected _ _ = throwAll err401
