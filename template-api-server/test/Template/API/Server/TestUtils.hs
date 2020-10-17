{-# LANGUAGE OverloadedStrings #-}

module Template.API.Server.TestUtils where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Database.Persist
import Database.Persist.Sqlite
import Network.HTTP.Client as HTTP
import Network.Wai.Handler.Warp as Warp
import Servant.Client
import Template.API
import Template.API.Server
import Template.API.Server.Env
import Test.Hspec

serverSpec :: SpecWith ClientEnv -> Spec
serverSpec = before (HTTP.newManager defaultManagerSettings) . aroundWith withTestServer

withTestServer :: (ClientEnv -> IO a) -> (HTTP.Manager -> IO a)
withTestServer func manager = do
  runNoLoggingT $ withSqlitePool ":memory:" 1 $ \pool -> do
    -- TODO do the migrations
    let serverEnv = Env {envConnectionPool = pool}
    let serverApp = templateAPIServerApp serverEnv
    liftIO $ testWithApplication (pure serverApp) $ \port -> do
      burl <- parseBaseUrl $ "127.0.0.1:" <> show port
      let env = mkClientEnv manager burl
      func env

testClient :: ClientEnv -> ClientM a -> IO a
testClient cenv func = do
  res <- runClientM func cenv
  case res of
    Left err -> failure $ show err
    Right r -> pure r

failure :: String -> IO a
failure err = do
  expectationFailure $ show err
  undefined "Won't get here anyway"
