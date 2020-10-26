{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Foobar.CLI
  ( foobarCLI,
  )
where

import Foobar.CLI.Command
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS as HTTP

foobarCLI :: IO ()
foobarCLI = do
  Instructions disp Settings {..} <- getInstructions
  mCenv <- forM settingBaseUrl $ \burl -> do
    man <- HTTP.newManager HTTP.tlsManagerSettings
    pure $ mkClientEnv man burl
  let env =
        Env
          { envClientEnv = mCenv,
            envUsername = settingUsername,
            envPassword = settingPassword
          }
  runReaderT (dispatch disp) env

dispatch :: Dispatch -> C ()
dispatch = \case
  DispatchRegister -> register
  DispatchLogin -> login
  DispatchGreet -> greet
