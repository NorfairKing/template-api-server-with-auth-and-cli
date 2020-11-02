{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Foo.Bar.API.Server.Handler.Greet where

import Foo.Bar.API.Server.Handler.Import

handleGetGreet :: AuthCookie -> H Text
handleGetGreet AuthCookie {..} = withUser authCookieUsername $ \_ ->
  pure $ "Hello " <> usernameText authCookieUsername
