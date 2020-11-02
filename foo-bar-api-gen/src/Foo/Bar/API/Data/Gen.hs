{-# OPTIONS_GHC -fno-warn-orphans #-}

module Foo.Bar.API.Data.Gen where

import Data.GenValidity
import Data.GenValidity.Text ()
import Foo.Bar.API.Data

instance GenValid RegistrationForm where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid LoginForm where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Username where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
