{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Foo.Bar.CLI.OptParse
  ( getInstructions,
    Instructions (..),
    Dispatch (..),
    Settings (..),
    getDefaultConfigFile,
  )
where

import Autodocodec
import Autodocodec.Yaml
import Control.Applicative
import Control.Arrow
import Data.Text (Text)
import qualified Data.Text as T
import qualified Env
import Foo.Bar.API.Data
import Options.Applicative as OptParse
import qualified Options.Applicative.Help as OptParse (pretty)
import Path
import Path.IO
import Servant.Client

data Instructions
  = Instructions !Dispatch !Settings

getInstructions :: IO Instructions
getInstructions = do
  args@(Arguments _ flags) <- getArguments
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToInstructions args env config

-- | A product type for the settings that are common across commands
data Settings = Settings
  { settingBaseUrl :: !(Maybe BaseUrl),
    settingUsername :: !(Maybe Username),
    settingPassword :: !(Maybe Text)
  }

-- | A sum type for the commands and their specific settings
data Dispatch
  = DispatchRegister
  | DispatchLogin
  | DispatchGreet

combineToInstructions :: Arguments -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions (Arguments cmd Flags {..}) Environment {..} mConf = do
  -- This is a typical way to combine a setting.
  --
  -- We choose the first of the supplied flag, environment variable or configuration field,
  -- or default value if none of the those were supplied.
  let settingBaseUrl = flagBaseUrl <|> envBaseUrl <|> mc configBaseUrl
  let settingUsername = flagUsername <|> envUsername <|> mc configUsername
  let settingPassword = flagPassword <|> envPassword <|> mc configPassword
  let sets = Settings {..}
  disp <-
    -- Resolve the command-specific settings for each command
    case cmd of
      CommandRegister -> pure DispatchRegister
      CommandLogin -> pure DispatchLogin
      CommandGreet -> pure DispatchGreet
  pure $ Instructions disp sets
  where
    mc :: (Configuration -> Maybe a) -> Maybe a
    mc f = mConf >>= f

getDefaultConfigFile :: IO (Path Abs File)
getDefaultConfigFile = do
  xdgConfigDir <- getXdgDir XdgConfig (Just [reldir|foo-bar|])
  resolveFile xdgConfigDir "config.yaml"

data Configuration = Configuration
  { configBaseUrl :: !(Maybe BaseUrl),
    configUsername :: !(Maybe Username),
    configPassword :: !(Maybe Text)
  }

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> optionalFieldWith "server-url" (bimapCodec (left show . parseBaseUrl) showBaseUrl codec) "Server base url"
          .= configBaseUrl
        <*> optionalField "username" "Server account username"
          .= configUsername
        <*> optionalField "password" "Server account password"
          .= configPassword

-- | Get the configuration
--
-- We use the flags and environment because they can contain information to override where to look for the configuration files.
-- We return a 'Maybe' because there may not be a configuration file.
getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} =
  case flagConfigFile <|> envConfigFile of
    Nothing -> getDefaultConfigFile >>= readYamlConfigFile
    Just cf -> do
      afp <- resolveFile' cf
      readYamlConfigFile afp

-- | What we find in the configuration variable.
--
-- Do nothing clever here, just represent the relevant parts of the environment.
-- For example, use 'Text', not 'SqliteConfig'.
data Environment = Environment
  { envConfigFile :: !(Maybe FilePath),
    envBaseUrl :: !(Maybe BaseUrl),
    envUsername :: !(Maybe Username),
    envPassword :: !(Maybe Text)
  }

getEnvironment :: IO Environment
getEnvironment = Env.parse (Env.header "Environment") environmentParser

-- | The 'envparse' parser for the 'Environment'
environmentParser :: Env.Parser Env.Error Environment
environmentParser =
  Env.prefixed "FOO_BAR_" $
    Environment
      <$> optional (Env.var Env.str "CONFIG_FILE" (Env.help "Config file"))
      <*> optional (Env.var (maybe (Left $ Env.unread "unable to parse base url") Right . parseBaseUrl) "SERVER_URL" (Env.help "Server base url"))
      <*> optional (Env.var (left Env.unread . parseUsernameOrErr . T.pack) "USERNAME" (Env.help "Server account username"))
      <*> optional (Env.var Env.str "PASSWORD" (Env.help "Server account password"))

-- | The combination of a command with its specific flags and the flags for all commands
data Arguments
  = Arguments !Command !Flags

-- | Get the command-line arguments
getArguments :: IO Arguments
getArguments = customExecParser prefs_ argParser

-- | The 'optparse-applicative' parsing preferences
prefs_ :: OptParse.ParserPrefs
prefs_ =
  -- I like these preferences. Use what you like.
  OptParse.defaultPrefs
    { OptParse.prefShowHelpOnError = True,
      OptParse.prefShowHelpOnEmpty = True
    }

-- | The @optparse-applicative@ parser for 'Arguments'
argParser :: OptParse.ParserInfo Arguments
argParser =
  OptParse.info
    (OptParse.helper <*> parseArgs)
    (OptParse.fullDesc <> OptParse.footerDoc (Just $ OptParse.pretty footerStr))
  where
    -- Show the variables from the environment that we parse and the config file format
    footerStr =
      unlines
        [ Env.helpDoc environmentParser,
          "",
          "Configuration file format:",
          T.unpack (renderColouredSchemaViaCodec @Configuration)
        ]

parseArgs :: OptParse.Parser Arguments
parseArgs = Arguments <$> parseCommand <*> parseFlags

-- | A sum type for the commands and their specific arguments
data Command
  = CommandRegister
  | CommandLogin
  | CommandGreet

parseCommand :: OptParse.Parser Command
parseCommand =
  OptParse.hsubparser $
    mconcat
      [ OptParse.command "register" parseCommandRegister,
        OptParse.command "login" parseCommandLogin,
        OptParse.command "greet" parseCommandGreet
      ]

parseCommandRegister :: OptParse.ParserInfo Command
parseCommandRegister = OptParse.info parser modifier
  where
    modifier = OptParse.fullDesc <> OptParse.progDesc "Register the user"
    parser = pure CommandRegister

parseCommandLogin :: OptParse.ParserInfo Command
parseCommandLogin = OptParse.info parser modifier
  where
    modifier = OptParse.fullDesc <> OptParse.progDesc "Log the user in"
    parser = pure CommandLogin

parseCommandGreet :: OptParse.ParserInfo Command
parseCommandGreet = OptParse.info parser modifier
  where
    modifier = OptParse.fullDesc <> OptParse.progDesc "Greet the user"
    parser = pure CommandGreet

-- | The flags that are common across commands.
data Flags = Flags
  { flagConfigFile :: !(Maybe FilePath),
    flagBaseUrl :: !(Maybe BaseUrl),
    flagUsername :: !(Maybe Username),
    flagPassword :: !(Maybe Text)
  }

-- | The 'optparse-applicative' parser for the 'Flags'.
parseFlags :: OptParse.Parser Flags
parseFlags =
  Flags
    <$> optional
      ( strOption
          ( mconcat
              [ long "config-file",
                help "Give the path to an altenative config file",
                metavar "FILEPATH"
              ]
          )
      )
    <*> optional
      ( option
          (maybeReader parseBaseUrl)
          ( mconcat
              [ long "server-url",
                help "Server base url"
              ]
          )
      )
    <*> optional
      ( option
          (eitherReader $ parseUsernameOrErr . T.pack)
          ( mconcat
              [ long "username",
                help "Server account username",
                metavar "USERNAME"
              ]
          )
      )
    <*> optional
      ( strOption
          ( mconcat
              [ long "password",
                help "Server account password",
                metavar "PASSWORD"
              ]
          )
      )
