{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Foobar.API.Server.Handler.Greet where

import Foobar.API.Server.Handler.Import

handleGetGreet :: AuthCookie -> H Text
handleGetGreet AuthCookie {..} = withUser authCookieUsername $ \_ ->
  pure $ "Hello " <> usernameText authCookieUsername
