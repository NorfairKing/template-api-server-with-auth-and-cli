{-# LANGUAGE TypeApplications #-}

module Foo.Bar.API.DataSpec
  ( spec,
  )
where

import Foo.Bar.API.Data
import Foo.Bar.API.Data.Gen ()
import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
  genValidSpec @RegistrationForm
  genValidSpec @LoginForm
