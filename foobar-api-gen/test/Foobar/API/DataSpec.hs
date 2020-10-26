{-# LANGUAGE TypeApplications #-}

module Foobar.API.DataSpec
  ( spec,
  )
where

import Foobar.API.Data
import Foobar.API.Data.Gen ()
import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
  genValidSpec @RegistrationForm
  genValidSpec @LoginForm
