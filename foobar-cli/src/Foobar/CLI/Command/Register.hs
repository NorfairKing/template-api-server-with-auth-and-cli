{-# LANGUAGE RecordWildCards #-}

module Foobar.CLI.Command.Register where

import Foobar.CLI.Command.Import

register :: C ()
register = withClient $ \cenv -> do
  registrationFormUsername <- getEnvUsername
  registrationFormPassword <- getEnvPassword
  let rf = RegistrationForm {..}
  NoContent <- runClientOrDie cenv $ postRegister foobarClient rf
  pure ()
