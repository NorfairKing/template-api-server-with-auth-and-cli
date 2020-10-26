{-# OPTIONS_GHC -fno-warn-orphans #-}

module Foobar.API.Data.Gen where

import Data.GenValidity
import Data.GenValidity.Text ()
import Foobar.API.Data

instance GenValid RegistrationForm where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid LoginForm where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Username where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
