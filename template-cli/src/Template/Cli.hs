{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Template.Cli where

import Control.Monad.Reader
import qualified Data.ByteString as SB
import qualified Data.ByteString.Builder as SBB
import qualified Data.ByteString.Lazy as LB
import Data.List
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS
import Path
import Path.IO
import System.Environment as System
import System.Exit
import Template.Client
import Template.Data
import Web.Cookie

templateCli :: IO ()
templateCli = do
  (cmd : _) <- System.getArgs
  burl <- parseBaseUrl "127.0.0.1:8000"
  un <- case parseUsername "foobar" of
    Nothing -> die "Invalid username"
    Just un -> pure un
  let pw = "hunter2"
  man <- HTTP.newManager tlsManagerSettings
  let cenv = mkClientEnv man burl
  let env = Env {envClientEnv = cenv}
  let func = case cmd of
        "example" -> example un pw
        "register" -> register un pw
        "login" -> login un pw
        "greet" -> greet
        _ -> liftIO $ die $ "Unknown command: " <> cmd
  runReaderT func env

example :: Username -> Text -> C ()
example un pw = do
  register un pw
  login un pw
  greet
  pure ()

register :: Username -> Text -> C ()
register un pw = do
  NoContent <-
    runClientOrDie $ postRegister templateClient $
      RegistrationForm
        { registrationFormUsername = un,
          registrationFormPassword = pw
        }
  pure ()

login :: Username -> Text -> C ()
login un pw = do
  Headers NoContent (HCons sessionHeader HNil) <-
    runClientOrDie $ postLogin templateClient $
      LoginForm
        { loginFormUsername = un,
          loginFormPassword = pw
        }
  case sessionHeader of
    MissingHeader ->
      liftIO $ die "The server responded but the response was missing the right session header."
    UndecodableHeader _ ->
      liftIO $ die "The server responded but the response had an undecodable session header."
    Header setCookieText -> do
      let cookies = parseSetCookie . TE.encodeUtf8 <$> T.lines setCookieText
          jwtCookie = find ((== "JWT-Cookie") . setCookieName) cookies
      case jwtCookie of
        Nothing -> liftIO $ die "No JWT-Cookie was found in the Set-Cookie session header."
        Just setCookie -> writeToken setCookie

greet :: C ()
greet = withTokenOrDie $ \token -> do
  t <- runClientOrDie $ getGreeting templateClient token (Just "HELLO")
  liftIO $ print t

writeToken :: SetCookie -> C ()
writeToken setCookie = do
  tokenPath <- liftIO getTokenPath
  liftIO $ do
    ensureDir $ parent tokenPath
    SB.writeFile (fromAbsFile tokenPath) $ LB.toStrict $ SBB.toLazyByteString $ renderSetCookie setCookie

withTokenOrDie :: (Token -> C ()) -> C ()
withTokenOrDie func = do
  mt <- readToken
  case mt of
    Nothing -> liftIO $ die "Please login first."
    Just t -> func t

readToken :: C (Maybe Token)
readToken = liftIO $ do
  tokenPath <- getTokenPath
  fmap Token <$> forgivingAbsence (SB.readFile (fromAbsFile tokenPath))

getTokenPath :: IO (Path Abs File)
getTokenPath = resolveFile' tokenFile

tokenFile :: FilePath
tokenFile = "token.dat"

data Env = Env {envClientEnv :: ClientEnv}

type C = ReaderT Env IO

runClientOrDie :: ClientM a -> C a
runClientOrDie func = do
  errOrRes <- runClient func
  case errOrRes of
    Left err -> liftIO $ die $ show err
    Right r -> pure r

runClient :: ClientM a -> C (Either ClientError a)
runClient func = do
  cenv <- asks envClientEnv
  liftIO $ runClientM func cenv
