{-# OPTIONS_GHC -fno-warn-orphans #-}

module Template.Data.Gen where

import Data.GenValidity
import Data.GenValidity.Text ()
import Template.Data

instance GenValid RegistrationForm where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid LoginForm where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Username where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
