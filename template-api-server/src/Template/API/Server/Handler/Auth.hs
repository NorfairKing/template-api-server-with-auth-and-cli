{-# LANGUAGE DataKinds #-}

module Template.API.Server.Handler.Auth where

import Template.API.Server.Handler.Import

handlePostRegister :: RegistrationForm -> H NoContent
handlePostRegister = undefined

handlePostLogin :: LoginForm -> H (Headers '[Header "Set-Cookie" Text] NoContent)
handlePostLogin = undefined
