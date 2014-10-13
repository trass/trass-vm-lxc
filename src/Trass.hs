{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Trass where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HashMap
import Data.Monoid
import Data.Function
import Data.Aeson.Types (Pair)
import Data.Yaml
import Data.Text (Text)
import qualified Data.Text as Text

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent

import System.LXC
import System.Exit
import System.Process
import System.Unix.Directory (withTemporaryDirectory)
import System.Posix.Files
import System.Posix.IO (handleToFd)

-- General configuration with options

type Options m = Map Text (Map Text (Configuration m))

data Configuration m = Configuration
  { configurationOptions :: Options m
  , configurationGlobal  :: m
  }
  deriving (Show)

data ConfigWithOptions m = ConfigWithOptions
  { configWithOptionsOptions  :: Map Text Text
  , configWithOptionsConfig   :: m
  }
  deriving (Show)

instance Monoid m => Monoid (Configuration m) where
  mempty = Configuration Map.empty mempty
  mappend t t' = Configuration
    (mergeOptions (configurationOptions t) (configurationOptions t'))
    (configurationGlobal t <> configurationGlobal t')

instance Monoid m => Monoid (ConfigWithOptions m) where
  mempty = ConfigWithOptions Map.empty mempty
  mappend c c' = ConfigWithOptions
    (configWithOptionsOptions c <> configWithOptionsOptions c')
    (configWithOptionsConfig  c <> configWithOptionsConfig  c')

instance (FromJSON m, Monoid m) => FromJSON (Configuration m) where
  parseJSON o@(Object v) = Configuration
                       <$> v .:  "options"
                       <*> v .:? "global"   .!= mempty

                       <|> Configuration Map.empty
                       <$> v .: "global"

                       <|> Configuration Map.empty
                       <$> parseJSON o
  parseJSON _ = empty

instance (FromJSON m, ToJSON m, Monoid m) => FromJSON (ConfigWithOptions m) where
  parseJSON v = do
    cfg  <- parseJSON v
    opts <- parseJSON v
    let keys = case toJSON cfg of
                 Object v' -> HashMap.keys v'
                 _         -> []
        opts' = foldr Map.delete opts keys
    return $ ConfigWithOptions opts' cfg

instance ToJSON m => ToJSON (Configuration m) where
  toJSON Configuration{..} = object
    [ "options" .= configurationOptions
    , "global"  .= configurationGlobal
    ]

mergeOptions :: Monoid m => Options m -> Options m -> Options m
mergeOptions = Map.unionWith (Map.unionWith (<>))

applyConfiguration :: Monoid m => Configuration m -> ConfigWithOptions m -> Either String m
applyConfiguration cfg (ConfigWithOptions opts m)
  | Map.null opts = Right $ configurationGlobal cfg <> m
  | null cfgs     = Left $ "unknown options: " <> show (Map.keys opts)
  | otherwise     = applyConfiguration cfg'' (ConfigWithOptions opts' m)
  where
    opts'   = Map.difference opts cfgOpts
    cfgOpts = configurationOptions cfg
    cfgs    = Map.elems $ Map.intersectionWith (Map.!) cfgOpts opts
    cfg'    = mconcat cfgs
    cfg''   = cfg' { configurationGlobal = configurationGlobal cfg <> configurationGlobal cfg' }

-- TRASS configurations

newtype Commands = Commands
  { getCommands :: [Text]
  } deriving (Show, Read, Monoid, ToJSON)

type EnvVars = Commands

instance FromJSON Commands where
  parseJSON (String cmd) = pure $ Commands [cmd]
  parseJSON v = Commands <$> parseJSON v

data TrassConfig = TrassConfig
  { trassConfigDist               :: Maybe Text
  , trassConfigRelease            :: Maybe Text
  , trassConfigArch               :: Maybe Text
  , trassConfigEnvironment        :: EnvVars
  , trassConfigContainer          :: TrassContainerConfig
  , trassConfigSubmission         :: TrassSubmissionConfig
  }
  deriving (Show)

data TrassContainerConfig = TrassContainerConfig
  { trassContainerConfigUser      :: TrassUserConfig
  , trassContainerConfigPrepare   :: Commands
  }
  deriving (Show)

data TrassSubmissionConfig = TrassSubmissionConfig
  { trassSubmissionConfigPrepare  :: Commands
  , trassSubmissionConfigValidate :: Commands
  , trassSubmissionConfigInstall  :: Commands
  , trassSubmissionConfigScript   :: Commands
  }
  deriving (Show)

data TrassUserConfig = TrassUserConfig
  { trassUserConfigUsername       :: Maybe Text
  , trassUserConfigHome           :: Maybe FilePath
  , trassUserConfigPrepare        :: Commands
  }
  deriving (Show)

lastOf :: a -> a -> (a -> Maybe b) -> Maybe b
lastOf x y f = maybe (f x) Just (f y)

mappendOf :: Monoid m => a -> a -> (a -> m) -> m
mappendOf x y f = f x <> f y

instance Monoid TrassConfig where
  mempty = TrassConfig Nothing Nothing Nothing mempty mempty mempty
  mappend c c' = TrassConfig
    (lastOf'    trassConfigDist)
    (lastOf'    trassConfigRelease)
    (lastOf'    trassConfigArch)
    (mappendOf' trassConfigEnvironment)
    (mappendOf' trassConfigContainer)
    (mappendOf' trassConfigSubmission)
    where
      lastOf'    = lastOf c c'
      mappendOf' = mappendOf c c'

instance Monoid TrassContainerConfig where
  mempty = TrassContainerConfig mempty mempty
  mappend c c' = TrassContainerConfig
    (mappendOf' trassContainerConfigUser)
    (mappendOf' trassContainerConfigPrepare)
    where
      mappendOf' = mappendOf c c'

instance Monoid TrassSubmissionConfig where
  mempty = TrassSubmissionConfig mempty mempty mempty mempty
  mappend c c' = TrassSubmissionConfig
    (mappendOf' trassSubmissionConfigPrepare)
    (mappendOf' trassSubmissionConfigValidate)
    (mappendOf' trassSubmissionConfigInstall)
    (mappendOf' trassSubmissionConfigScript)
    where
      mappendOf' = mappendOf c c'

instance Monoid TrassUserConfig where
  mempty = TrassUserConfig Nothing Nothing mempty
  mappend c c' = TrassUserConfig
    (lastOf'    trassUserConfigUsername)
    (lastOf'    trassUserConfigHome)
    (mappendOf' trassUserConfigPrepare)
    where
      lastOf'    = lastOf c c'
      mappendOf' = mappendOf c c'

instance FromJSON TrassConfig where
  parseJSON (Object v) = TrassConfig
                     <$> v .:? "dist"
                     <*> v .:? "release"
                     <*> v .:? "arch"
                     <*> v .:? "env" .!= mempty
                     <*> v .:? "container"  .!= mempty
                     <*> v .:? "submission" .!= mempty

instance FromJSON TrassContainerConfig where
  parseJSON (Object v) = TrassContainerConfig
                     <$> v .:? "user"    .!= mempty
                     <*> v .:? "prepare" .!= mempty
  parseJSON v@(Array _) = TrassContainerConfig mempty <$> parseJSON v
  parseJSON _ = empty

instance FromJSON TrassSubmissionConfig where
  parseJSON (Object v) = TrassSubmissionConfig
                     <$> v .:? "prepare"  .!= mempty
                     <*> v .:? "validate" .!= mempty
                     <*> v .:? "install"  .!= mempty
                     <*> v .:? "script"   .!= mempty

instance FromJSON TrassUserConfig where
  parseJSON (Object v) = TrassUserConfig
                     <$> v .:? "username"
                     <*> v .:? "home"
                     <*> v .:? "prepare" .!= mempty
  parseJSON _ = empty

(.=?) :: ToJSON a => Text -> Maybe a -> [Pair]
k .=? v = maybe [] (\v' -> [k .= v']) v

instance ToJSON TrassConfig where
  toJSON TrassConfig{..} = object $ concat
    [ "dist"          .=? trassConfigDist
    , "release"       .=? trassConfigRelease
    , "arch"          .=? trassConfigArch
    , [ "env"         .=  trassConfigEnvironment
      , "container"   .=  trassConfigContainer
      , "submission"  .=  trassConfigSubmission ] ]

instance ToJSON TrassContainerConfig where
  toJSON TrassContainerConfig{..} = object
    [ "user"    .= trassContainerConfigUser
    , "prepare" .= trassContainerConfigPrepare ]

instance ToJSON TrassSubmissionConfig where
  toJSON TrassSubmissionConfig{..} = object
    [ "prepare"   .= trassSubmissionConfigPrepare
    , "validate"  .= trassSubmissionConfigValidate
    , "install"   .= trassSubmissionConfigInstall
    , "script"    .= trassSubmissionConfigScript ]

instance ToJSON TrassUserConfig where
  toJSON TrassUserConfig{..} = object $ concat
    [ "username"  .=? trassUserConfigUsername
    , "home"      .=? trassUserConfigHome
    , [ "prepare" .=  trassUserConfigPrepare ] ]

attach' :: Text -> LXC (Maybe ExitCode)
attach' cmd = do
  let cmd' = Text.unpack cmd
  liftIO $ putStrLn ("$ " ++ cmd')
  attachRunWait defaultAttachOptions "sh" ["sh", "-c", cmd']

attachMany :: Commands -> LXC (Maybe ExitCode)
attachMany (Commands []) = return (Just ExitSuccess)
attachMany (Commands (cmd:cmds)) = do
  mc <- attach' cmd
  case mc of
    Just ExitSuccess -> attachMany (Commands cmds)
    _ -> return mc

copyToContainer :: FilePath -> FilePath -> LXC ()
copyToContainer hostPath containerPath = do
  outFd <- liftIO $ do
    (_, Just hout, _, _) <- createProcess (proc "tar" ["zcf", "-", hostPath]){ std_out = CreatePipe }
    handleToFd hout
  attachRunWait
    defaultAttachOptions { attachStdinFD = outFd }
    "sh" ["sh", "-c", "tar zxf - -O >" ++ containerPath]
  return ()

