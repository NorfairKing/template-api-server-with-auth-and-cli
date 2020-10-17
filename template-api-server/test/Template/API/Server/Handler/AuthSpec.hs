module Template.API.Server.Handler.AuthSpec (spec) where

import Network.HTTP.Types as HTTP
import Template.API
import Template.API.Server.TestUtils
import Template.Client
import Template.Data
import Template.Data.Gen ()
import Test.Hspec
import Test.Validity

spec :: Spec
spec = serverSpec $ do
  describe "PostRegister" $ do
    it "does not crash" $ \cenv ->
      forAllValid $ \rf -> do
        NoContent <- testClientOrErr cenv $ postRegister templateClient rf
        pure ()
  describe "PostRegister" $ do
    it "fails before registration" $ \cenv ->
      forAllValid $ \lf -> do
        errOrRes <- testClient cenv $ postLogin templateClient lf
        case errOrRes of
          Left err -> case err of
            FailureResponse _ resp | responseStatusCode resp == HTTP.unauthorized401 -> pure ()
            _ -> failure "Should have errored with code 401"
          _ -> failure "Should have errored"
    it "succeeds after registration" $ \cenv ->
      forAllValid $ \rf -> do
        _ <- testClientOrErr cenv $ do
          postRegister templateClient rf
          let lf =
                LoginForm
                  { loginFormUsername = registrationFormUsername rf,
                    loginFormPassword = registrationFormPassword rf
                  }
          postLogin templateClient lf
        pure ()
