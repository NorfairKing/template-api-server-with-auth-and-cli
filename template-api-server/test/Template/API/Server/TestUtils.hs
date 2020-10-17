{-# LANGUAGE OverloadedStrings #-}

module Template.API.Server.TestUtils where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Database.Persist
import Database.Persist.Sqlite
import Network.HTTP.Client as HTTP
import Network.Wai.Handler.Warp as Warp
import Servant.Auth.Server
import Servant.Client
import Template.API
import Template.API.Server
import Template.API.Server.DB
import Template.API.Server.Env
import Test.Hspec
import Test.Hspec.QuickCheck

serverSpec :: SpecWith ClientEnv -> Spec
serverSpec =
  before (HTTP.newManager defaultManagerSettings) . aroundWith withTestServer
    . modifyMaxSuccess (`div` 20)
    . modifyMaxShrinks (const 0) -- Shrinks are broken when using 'around'

withTestServer :: (ClientEnv -> IO a) -> (HTTP.Manager -> IO a)
withTestServer func manager = do
  runNoLoggingT $ withSqlitePool ":memory:" 1 $ \pool -> do
    runSqlPool (runMigrationQuiet migrateAll) pool
    liftIO $ do
      jwk <- generateKey
      let serverEnv =
            Env
              { envConnectionPool = pool,
                envCookieSettings = defaultCookieSettings,
                envJWTSettings = defaultJWTSettings jwk
              }
      let serverApp = templateAPIServerApp serverEnv
      testWithApplication (pure serverApp) $ \port -> do
        let env = mkClientEnv manager $ BaseUrl Http "127.0.0.1" port ""
        func env

testClientOrErr :: ClientEnv -> ClientM a -> IO a
testClientOrErr cenv func = do
  res <- runClientM func cenv
  case res of
    Left err -> failure $ show err
    Right r -> pure r

testClient :: ClientEnv -> ClientM a -> IO (Either ClientError a)
testClient = flip runClientM

failure :: String -> IO a
failure err = do
  expectationFailure $ show err
  undefined "Won't get here anyway"
