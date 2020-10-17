{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Template.API.Server.Handler.Greet where

import Data.Maybe
import Template.API.Server.Handler.Import

handleGetGreeting :: AuthCookie -> Maybe Text -> H Text
handleGetGreeting AuthCookie {..} greeting = pure $ fromMaybe "Hello" greeting <> " " <> usernameText authCookieUsername
