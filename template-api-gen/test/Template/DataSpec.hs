{-# LANGUAGE TypeApplications #-}

module Template.DataSpec
  ( spec,
  )
where

import Template.Data
import Template.Data.Gen ()
import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
  genValidSpec @RegistrationForm
  genValidSpec @LoginForm
  genValidSpec @Username
