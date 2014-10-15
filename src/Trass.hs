{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Trass where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HashMap
import Data.Monoid
import Data.Maybe
import Data.Function
import Data.Aeson.Types (Pair, Value(..))
import Data.Yaml
import Data.Text (Text)
import qualified Data.Text as Text

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent

import System.LXC
import System.Exit
import System.FilePath
import System.Process
import System.Unix.Directory (withTemporaryDirectory)
import System.Posix.Files
import System.Posix.IO (handleToFd)

-- General configuration with options

newtype TextValue = TextValue
  { getTextValue :: Text
  } deriving (Eq, Ord, Show, Read, Monoid, ToJSON)

instance FromJSON TextValue where
  parseJSON (String s) = pure $ TextValue s
  parseJSON (Number n) = pure $ TextValue (Text.pack $ show n)
  parseJSON (Bool b)   = pure $ TextValue (if b then "true" else "false")
  parseJSON Null       = pure $ TextValue "null"
  parseJSON v          = pure $ TextValue (read . show $ encode v)

type OptionKey   = Text
type OptionValue = TextValue
type Options m   = Map OptionKey (Map OptionValue (Configuration m))

instance FromJSON v => FromJSON (Map TextValue v) where
  parseJSON v = Map.mapKeys TextValue <$> parseJSON v

instance ToJSON v => ToJSON (Map TextValue v) where
  toJSON = toJSON . Map.mapKeys getTextValue

data Configuration m = Configuration
  { configurationOptions  :: Options m
  , configurationDefault  :: Map OptionKey OptionValue
  , configurationGlobal   :: m
  }
  deriving (Show)

data ConfigWithOptions m = ConfigWithOptions
  { configWithOptionsOptions  :: Map OptionKey OptionValue
  , configWithOptionsConfig   :: m
  }
  deriving (Show)

instance Monoid m => Monoid (Configuration m) where
  mempty = Configuration Map.empty Map.empty mempty
  mappend t t' = Configuration
    (mergeOptions (configurationOptions t) (configurationOptions t'))
    (configurationDefault t <> configurationDefault t')
    (configurationGlobal t <> configurationGlobal t')

instance Monoid m => Monoid (ConfigWithOptions m) where
  mempty = ConfigWithOptions Map.empty mempty
  mappend c c' = ConfigWithOptions
    (configWithOptionsOptions c <> configWithOptionsOptions c')
    (configWithOptionsConfig  c <> configWithOptionsConfig  c')

instance (FromJSON m, Monoid m) => FromJSON (Configuration m) where
  parseJSON o@(Object v) = Configuration
                       <$> v .:  "options"
                       <*> v .:? "default"  .!= Map.empty
                       <*> v .:? "global"   .!= mempty

                       <|> Configuration Map.empty Map.empty
                       <$> v .: "global"

                       <|> Configuration Map.empty Map.empty
                       <$> parseJSON o
  parseJSON v = Configuration Map.empty Map.empty <$> parseJSON v

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
    , "default" .= configurationDefault
    , "global"  .= configurationGlobal
    ]

mergeOptions :: Monoid m => Options m -> Options m -> Options m
mergeOptions = Map.unionWith (Map.unionWith (<>))

applyConfiguration :: Monoid m => Configuration m -> ConfigWithOptions m -> Either String m
applyConfiguration cfg (ConfigWithOptions opts m)
  | Map.null opts' = Right $ configurationGlobal cfg <> m
  | null cfgs      = Left $ "unknown options: " <> show (Map.keys opts')
  | otherwise      = applyConfiguration cfg'' (ConfigWithOptions opts'' m)
  where
    opts'   = configurationDefault cfg <> opts
    opts''  = Map.difference opts' cfgOpts
    cfgOpts = configurationOptions cfg
    cfgs    = Map.elems $ Map.intersectionWith (Map.!) cfgOpts opts'
    cfg'    = mconcat cfgs
    cfg''   = cfg' { configurationGlobal = configurationGlobal cfg <> configurationGlobal cfg' }

-- TRASS configurations

type Command = TextValue

newtype Commands = Commands
  { getCommands :: [Command]
  } deriving (Show, Read, Monoid, ToJSON)

type EnvVars = Commands

instance FromJSON Commands where
  parseJSON v = Commands <$> (parseJSON v <|> (\c -> [c]) <$> parseJSON v)

data TrassConfig = TrassConfig
  { trassConfigDist               :: Maybe Text
  , trassConfigRelease            :: Maybe Text
  , trassConfigArch               :: Maybe Text
  , trassConfigEnvironment        :: EnvVars
  , trassConfigPrepare            :: Commands
  , trassConfigUser               :: TrassUserConfig
  , trassConfigTaskDir            :: Maybe FilePath
  , trassConfigSubmission         :: TrassSubmissionConfig
  }
  deriving (Show)

data TrassSubmissionConfig = TrassSubmissionConfig
  { trassSubmissionConfigFile           :: Maybe FilePath
  , trassSubmissionConfigDir            :: Maybe FilePath
  , trassSubmissionConfigBeforeInstall  :: Maybe Commands
  , trassSubmissionConfigInstall        :: Maybe Commands
  , trassSubmissionConfigBeforeScript   :: Maybe Commands
  , trassSubmissionConfigScript         :: Maybe Commands
  , trassSubmissionConfigAfterSuccess   :: Maybe Commands
  , trassSubmissionConfigAfterFailure   :: Maybe Commands
  , trassSubmissionConfigAfterScript    :: Maybe Commands
  }
  deriving (Show)

data TrassUserConfig = TrassUserConfig
  { trassUserConfigUsername       :: Maybe Text
  , trassUserConfigHome           :: Maybe FilePath
  , trassUserConfigPrepare        :: Commands
  }
  deriving (Show)

lastOf :: a -> a -> (a -> Maybe b) -> Maybe b
lastOf x y f = f y <|> f x

mappendOf :: Monoid m => a -> a -> (a -> m) -> m
mappendOf x y f = f x <> f y

instance Monoid TrassConfig where
  mempty = TrassConfig Nothing Nothing Nothing mempty mempty mempty Nothing mempty
  mappend c c' = TrassConfig
    (lastOf'    trassConfigDist)
    (lastOf'    trassConfigRelease)
    (lastOf'    trassConfigArch)
    (mappendOf' trassConfigEnvironment)
    (mappendOf' trassConfigPrepare)
    (mappendOf' trassConfigUser)
    (lastOf'    trassConfigTaskDir)
    (mappendOf' trassConfigSubmission)
    where
      lastOf'    = lastOf c c'
      mappendOf' = mappendOf c c'

instance Monoid TrassSubmissionConfig where
  mempty = TrassSubmissionConfig Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
  mappend c c' = TrassSubmissionConfig
    (lastOf' trassSubmissionConfigFile)
    (lastOf' trassSubmissionConfigDir)
    (lastOf' trassSubmissionConfigBeforeInstall)
    (lastOf' trassSubmissionConfigInstall)
    (lastOf' trassSubmissionConfigBeforeScript)
    (lastOf' trassSubmissionConfigScript)
    (lastOf' trassSubmissionConfigAfterSuccess)
    (lastOf' trassSubmissionConfigAfterFailure)
    (lastOf' trassSubmissionConfigAfterScript)
    where
      lastOf'    = lastOf c c'

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
  parseJSON o@(Object v) = TrassConfig
                       <$> v .:? "dist"
                       <*> v .:? "release"
                       <*> v .:? "arch"
                       <*> v .:? "env"        .!= mempty
                       <*> v .:? "prepare"    .!= mempty
                       <*> v .:? "user"       .!= mempty
                       <*> v .:? "task_dir"
                       <*> (parseJSON o <|> pure mempty)
  parseJSON v = (\cmds -> mempty {trassConfigPrepare = cmds}) <$> parseJSON v

instance FromJSON TrassSubmissionConfig where
  parseJSON (Object v) = TrassSubmissionConfig
                     <$> v .:? "submit_file"
                     <*> v .:? "submit_dir"
                     <*> v .:? "before_install"
                     <*> v .:? "install"
                     <*> v .:? "before_script"
                     <*> v .:? "script"
                     <*> v .:? "after_success"
                     <*> v .:? "after_failure"
                     <*> v .:? "after_script"
  parseJSON _ = empty

instance FromJSON TrassUserConfig where
  parseJSON (Object v) = TrassUserConfig
                     <$> v .:? "username"
                     <*> v .:? "home"
                     <*> v .:? "prepare" .!= mempty
  parseJSON _ = empty

(.=?) :: ToJSON a => Text -> Maybe a -> [Pair]
k .=? v = maybe [] (\v' -> [k .= v']) v

objectUnion :: Value -> Value -> Value
objectUnion (Object v) (Object v') = Object $ HashMap.union v v'
objectUnion _ _ = object []

instance ToJSON TrassConfig where
  toJSON TrassConfig{..} = objectUnion
    (object $ concat
      [ "dist"          .=? trassConfigDist
      , "release"       .=? trassConfigRelease
      , "arch"          .=? trassConfigArch
      , [ "env"         .=  trassConfigEnvironment
        , "prepare"     .=  trassConfigPrepare
        , "user"        .=  trassConfigUser ]
      , "task_dir"      .=? trassConfigTaskDir ] )
    (toJSON trassConfigSubmission)

instance ToJSON TrassSubmissionConfig where
  toJSON TrassSubmissionConfig{..} = object $ concat
    [ "submit_file"     .=? trassSubmissionConfigFile
    , "submit_dir"      .=? trassSubmissionConfigDir
    , "before_install"  .=? trassSubmissionConfigBeforeInstall
    , "install"         .=? trassSubmissionConfigInstall
    , "before_script"   .=? trassSubmissionConfigBeforeScript
    , "script"          .=? trassSubmissionConfigScript
    , "after_success"   .=? trassSubmissionConfigAfterSuccess
    , "after_failure"   .=? trassSubmissionConfigAfterFailure
    , "after_script"    .=? trassSubmissionConfigAfterScript ]

instance ToJSON TrassUserConfig where
  toJSON TrassUserConfig{..} = object $ concat
    [ "username"  .=? trassUserConfigUsername
    , "home"      .=? trassUserConfigHome
    , [ "prepare" .=  trassUserConfigPrepare ] ]

attach' :: [String] -> TrassUserConfig -> Command -> LXC (Maybe ExitCode)
attach' env TrassUserConfig{..} cmd = do
  let run   = attachRunWait defaultAttachOptions { attachExtraEnvVars = env }
      cmd'  = Text.unpack (getTextValue cmd)
      cmd'' = case trassUserConfigHome of
                Nothing   -> cmd'
                Just path -> "cd " <> path <> ";" <> cmd'
  liftIO $ putStrLn ("$ " ++ cmd')
  case trassUserConfigUsername of
    Nothing -> run "sh" [ "sh", "-c", cmd'' ]
    Just u  -> run "su" [ "su", Text.unpack u, "-c", cmd'' ]

attachMany :: [String] -> TrassUserConfig -> Commands -> LXC (Maybe ExitCode)
attachMany _ _ (Commands []) = return (Just ExitSuccess)
attachMany env user (Commands (cmd:cmds)) = do
  mc <- attach' env user cmd
  case mc of
    Just ExitSuccess -> attachMany env user (Commands cmds)
    _ -> return mc

hostToContainer :: CreateProcess -> String -> LXC ()
hostToContainer hostProc containerCmd = do
  outFd <- liftIO $ do
    (_, Just hout, _, _) <- createProcess hostProc { std_out = CreatePipe }
    handleToFd hout
  attachRunWait
    defaultAttachOptions { attachStdinFD = outFd }
    "sh" ["sh", "-c", containerCmd]
  return ()

fileToFile :: FilePath -> FilePath -> LXC ()
fileToFile from to = hostToContainer (proc "cat" [from]) ("cat >" ++ to)

dirToDir :: FilePath -> FilePath -> LXC ()
dirToDir from to = hostToContainer
  (proc "tar" ["-p", "-C", from, "-zcf", "-", "."])
  ("mkdir -p " ++ show to ++ " && tar -p -C " ++ show to ++ " -zxf -")

archiveToDir :: FilePath -> FilePath -> LXC ()
archiveToDir from to = hostToContainer
  (proc "cat" [from])
  ("mkdir -p " ++ show to ++ " && tar -p -C " ++ show to ++ " -zxf -")

prepareContainer :: TrassConfig -> LXC Bool
prepareContainer TrassConfig{..} = do
  case (,,) <$> trassConfigDist <*> trassConfigRelease <*> trassConfigArch of
    Nothing -> return False
    Just (dist, release, arch) -> do
      create "download" Nothing Nothing [] $ map Text.unpack ["-d", dist, "-r", release, "-a", arch]
      start False []
      wait ContainerRunning (-1)

      -- get env
      let env  = getCommands trassConfigEnvironment <> [TextValue $ "USER=" <> fromMaybe "root" (trassUserConfigUsername trassConfigUser)]
          env' = map (Text.unpack . getTextValue) env
      -- prepare user
      attachMany env' mempty (trassUserConfigPrepare trassConfigUser)
      -- prepare container
      attachMany env' trassConfigUser trassConfigPrepare

      stop
      wait ContainerStopped (-1)

      return True

submit :: Container -> FilePath -> FilePath -> TrassConfig -> IO (Maybe ExitCode)
submit base submitFile taskDir TrassConfig{..} = do
  let TrassSubmissionConfig{..} = trassConfigSubmission
  withTemporaryDirectory "submission." $ \tempdir -> do
    setFileMode tempdir accessModes
    mc <- withContainer base $ clone Nothing (Just tempdir) [CloneSnapshot] Nothing Nothing Nothing []
    case mc of
      Nothing   -> return Nothing
      Just temp -> withContainer temp $ do
        liftIO $ print temp
        liftIO $ putStrLn "starting"
        start False []
        wait ContainerRunning (-1)
        liftIO $ putStrLn "started"

        let homeDir     = fromMaybe "" $ trassUserConfigHome trassConfigUser
            taskDir'    = fromMaybe "trass_task_dir"    trassConfigTaskDir
            submitFile' = fromMaybe "trass_submit_file" trassSubmissionConfigFile

        liftIO $ putStrLn "copying task dir"
        dirToDir   taskDir    $ homeDir </> taskDir'

        liftIO $ putStrLn "copying submission file"
        fileToFile submitFile $ homeDir </> taskDir' </> submitFile'

        liftIO $ putStrLn "copied files"

        let env  = getCommands trassConfigEnvironment <> [TextValue $ "USER=" <> fromMaybe "root" (trassUserConfigUsername trassConfigUser)]
            env' = map (Text.unpack . getTextValue) env
            attachMany' = attachMany env' trassConfigUser { trassUserConfigHome = (</> taskDir') <$> trassUserConfigHome trassConfigUser }

        mcode <- attachMany' . mconcat . map (fromMaybe mempty) $
                    [ trassSubmissionConfigBeforeInstall
                    , trassSubmissionConfigInstall
                    , trassSubmissionConfigBeforeScript
                    , trassSubmissionConfigScript ]

        case mcode of
          Just code -> do
            attachMany' . fromMaybe mempty $
              case code of
                ExitSuccess -> trassSubmissionConfigAfterSuccess
                _           -> trassSubmissionConfigAfterFailure
            attachMany' $ fromMaybe mempty trassSubmissionConfigAfterScript
          _ -> return Nothing

        stop
        wait ContainerStopped (-1)
        destroy
        return mcode

