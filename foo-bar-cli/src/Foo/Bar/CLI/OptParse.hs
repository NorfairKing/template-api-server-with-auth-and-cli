{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Foo.Bar.CLI.OptParse
  ( getInstructions,
    Instructions (..),
    Dispatch (..),
    Settings (..),
    getDefaultClientDatabase,
    getDefaultDataDir,
    getDefaultConfigFile,
  )
where

import Control.Applicative
import Control.Arrow
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml
import qualified Env
import Foo.Bar.API.Data
import GHC.Generics (Generic)
import Options.Applicative as OptParse
import qualified Options.Applicative.Help as OptParse (string)
import Path
import Path.IO
import Servant.Client
import YamlParse.Applicative as YamlParse

data Instructions
  = Instructions Dispatch Settings
  deriving (Show, Eq, Generic)

getInstructions :: IO Instructions
getInstructions = do
  args@(Arguments _ flags) <- getArguments
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToInstructions args env config

-- | A product type for the settings that are common across commands
data Settings
  = Settings
      { settingBaseUrl :: Maybe BaseUrl,
        settingUsername :: Maybe Username,
        settingPassword :: Maybe Text
      }
  deriving (Show, Eq, Generic)

-- | A sum type for the commands and their specific settings
data Dispatch
  = DispatchRegister
  | DispatchLogin
  | DispatchGreet
  deriving (Show, Eq, Generic)

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

getDefaultClientDatabase :: IO (Path Abs File)
getDefaultClientDatabase = do
  dataDir <- getDefaultDataDir
  resolveFile dataDir "repetition-data.sqlite3"

getDefaultDataDir :: IO (Path Abs Dir)
getDefaultDataDir = getXdgDir XdgData (Just [reldir|foo-bar|])

getDefaultConfigFile :: IO (Path Abs File)
getDefaultConfigFile = do
  xdgConfigDir <- getXdgDir XdgConfig (Just [reldir|foo-bar|])
  resolveFile xdgConfigDir "config.yaml"

data Configuration
  = Configuration
      { configBaseUrl :: Maybe BaseUrl,
        configUsername :: Maybe Username,
        configPassword :: Maybe Text
      }
  deriving (Show, Eq, Generic)

instance FromJSON Configuration where
  parseJSON = viaYamlSchema

instance YamlSchema Configuration where
  yamlSchema =
    objectParser "Configuration" $
      Configuration
        <$> optionalFieldWith "server-url" "Server base url" (maybeParser parseBaseUrl yamlSchema)
        <*> optionalField "username" "Server account username"
        <*> optionalField "password" "Server account password"

-- | Get the configuration
--
-- We use the flags and environment because they can contain information to override where to look for the configuration files.
-- We return a 'Maybe' because there may not be a configuration file.
getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} =
  case flagConfigFile <|> envConfigFile of
    Nothing -> getDefaultConfigFile >>= YamlParse.readConfigFile
    Just cf -> do
      afp <- resolveFile' cf
      YamlParse.readConfigFile afp

-- | What we find in the configuration variable.
--
-- Do nothing clever here, just represent the relevant parts of the environment.
-- For example, use 'Text', not 'SqliteConfig'.
data Environment
  = Environment
      { envConfigFile :: Maybe FilePath,
        envBaseUrl :: Maybe BaseUrl,
        envUsername :: Maybe Username,
        envPassword :: Maybe Text
      }
  deriving (Show, Eq, Generic)

getEnvironment :: IO Environment
getEnvironment = Env.parse (Env.header "Environment") environmentParser

-- | The 'envparse' parser for the 'Environment'
environmentParser :: Env.Parser Env.Error Environment
environmentParser =
  Env.prefixed "FOO_BAR_" $
    Environment
      <$> Env.var (fmap Just . Env.str) "CONFIG_FILE" (mE <> Env.help "Config file")
      <*> Env.var (fmap Just . maybe (Left $ Env.unread "unable to parse base url") Right . parseBaseUrl) "SERVER_URL" (mE <> Env.help "Server base url")
      <*> Env.var (fmap Just . left Env.unread . parseUsernameOrErr . T.pack) "USERNAME" (mE <> Env.help "Server account username")
      <*> Env.var (fmap Just . Env.str) "PASSWORD" (mE <> Env.help "Server account password")
  where
    mE = Env.def Nothing

-- | The combination of a command with its specific flags and the flags for all commands
data Arguments
  = Arguments Command Flags
  deriving (Show, Eq, Generic)

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
    (OptParse.fullDesc <> OptParse.footerDoc (Just $ OptParse.string footerStr))
  where
    -- Show the variables from the environment that we parse and the config file format
    footerStr =
      unlines
        [ Env.helpDoc environmentParser,
          "",
          "Configuration file format:",
          T.unpack (YamlParse.prettyColourisedSchemaDoc @Configuration)
        ]

parseArgs :: OptParse.Parser Arguments
parseArgs = Arguments <$> parseCommand <*> parseFlags

-- | A sum type for the commands and their specific arguments
data Command
  = CommandRegister
  | CommandLogin
  | CommandGreet
  deriving (Show, Eq, Generic)

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
data Flags
  = Flags
      { flagConfigFile :: Maybe FilePath,
        flagBaseUrl :: Maybe BaseUrl,
        flagUsername :: Maybe Username,
        flagPassword :: Maybe Text
      }
  deriving (Show, Eq, Generic)

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
