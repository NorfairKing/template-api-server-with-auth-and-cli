{-# LANGUAGE RecordWildCards #-}

module Foo.Bar.CLI.Command.Register where

import Foo.Bar.CLI.Command.Import

register :: C ()
register = withClient $ \cenv -> do
  registrationFormUsername <- getEnvUsername
  registrationFormPassword <- getEnvPassword
  let rf = RegistrationForm {..}
  NoContent <- runClientOrDie cenv $ postRegister fooBarClient rf
  pure ()
