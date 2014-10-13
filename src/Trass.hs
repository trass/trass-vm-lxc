{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Trass where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Function
import Data.Yaml

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

type Command = String

data TrassVariant = TrassVariant
  { trassVariantDist              :: Maybe String
  , trassVariantRelease           :: Maybe String
  , trassVariantArch              :: Maybe String
  , trassVariantEnvironment       :: [String]
  , trassVariantPrepareContainer  :: [Command]
  , trassVariantPrepareSubmit     :: [Command]
  , trassVariantValidate          :: [Command]
  , trassVariantInstall           :: [Command]
  , trassVariantScript            :: [Command]
  }
  deriving (Show)

emptyTrassVariant :: TrassVariant
emptyTrassVariant = TrassVariant Nothing Nothing Nothing [] [] [] [] [] []

instance Monoid TrassVariant where
  mempty = emptyTrassVariant
  mappend t t' = TrassVariant
    (lastOf trassVariantDist)
    (lastOf trassVariantRelease)
    (lastOf trassVariantArch)
    (mappendOf trassVariantEnvironment)
    (mappendOf trassVariantPrepareContainer)
    (mappendOf trassVariantPrepareSubmit)
    (mappendOf trassVariantValidate)
    (mappendOf trassVariantInstall)
    (mappendOf trassVariantScript)
    where
      lastOf f = getLast (Last (f t) <> Last (f t'))
      mappendOf f = f t <> f t'

instance FromJSON TrassVariant where
  parseJSON (Object v) = TrassVariant
                     <$> v .:? "dist"
                     <*> v .:? "release"
                     <*> v .:? "arch"
                     <*> v .:? "env"                .!= []
                     <*> v .:? "prepare_container"  .!= []
                     <*> v .:? "prepare_submit"     .!= []
                     <*> v .:? "validate"           .!= []
                     <*> v .:? "install"            .!= []
                     <*> v .:? "script"             .!= []
  parseJSON _ = empty

instance ToJSON TrassVariant where
  toJSON TrassVariant{..} = object
    [ "dist"              .= trassVariantDist
    , "release"           .= trassVariantRelease
    , "arch"              .= trassVariantArch
    , "env"               .= trassVariantEnvironment
    , "prepare_container" .= trassVariantPrepareContainer
    , "prepare_submit"    .= trassVariantPrepareSubmit
    , "validate"          .= trassVariantValidate
    , "install"           .= trassVariantInstall
    , "script"            .= trassVariantScript ]

data TrassConfig = TrassConfig
  { trassConfigGlobal   :: TrassVariant
  , trassConfigVariants :: Map String TrassVariant
  }
  deriving (Show)

instance FromJSON TrassConfig where
   parseJSON (Object v) = TrassConfig
                      <$> v .: "global"
                      <*> v .:? "variants" .!= Map.empty
   parseJSON _          = empty

instance ToJSON TrassConfig where
  toJSON TrassConfig{..} = object
    [ "global"    .= trassConfigGlobal
    , "variants"  .= trassConfigVariants ]

attach' :: String -> LXC (Maybe ExitCode)
attach' cmd = do
  liftIO $ putStrLn ("$ " ++ cmd)
  attachRunWait defaultAttachOptions "sh" ["sh", "-c", cmd]

attachMany :: [String] -> LXC (Maybe ExitCode)
attachMany [] = return (Just ExitSuccess)
attachMany (cmd:cmds) = do
  mc <- attach' cmd
  case mc of
    Just ExitSuccess -> attachMany cmds
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

prepareContainers :: String -> TrassConfig -> LXC () -> IO ()
prepareContainers prefix TrassConfig{..} prepare = do
  let variants = if Map.null trassConfigVariants
                   then Map.fromList [("", mempty)]
                   else trassConfigVariants
  forM_ (Map.toList variants) $ \(name, variant) -> do
    withContainer (Container (prefix ++ name) Nothing) $ do
      prepareContainer (trassConfigGlobal <> variant) prepare
  return ()

prepareContainer :: TrassVariant -> LXC () -> LXC (Maybe ExitCode)
prepareContainer TrassVariant{..} prepare = do
  case (,,) <$> trassVariantDist <*> trassVariantRelease <*> trassVariantArch of
    Nothing -> return Nothing
    Just (d, r, a) -> do
      create "download" Nothing Nothing [] ["-d", d, "-r", r, "-a", a]
      start False []
      wait ContainerRunning (-1)
      prepare
      code <- attachMany $ concat
                [ map (\var -> "echo 'export " ++ var ++ "' >> /etc/profile") trassVariantEnvironment
                , trassVariantPrepareContainer ]
      stop
      wait ContainerStopped (-1)
      return code

runSubmission :: Container -> TrassVariant -> LXC () -> IO (Maybe ExitCode)
runSubmission c TrassVariant{..} prepare = liftIO $ do
  withTemporaryDirectory "submission." $ \tempdir -> do
    setFileMode tempdir accessModes
    msc <- withContainer c $ clone Nothing (Just tempdir) [CloneSnapshot] Nothing Nothing Nothing []
    case msc of
      Nothing -> return Nothing
      Just sc -> withContainer sc $ do
        start False []
        wait ContainerRunning (-1)
        code <- attachMany $ concat
          [ trassVariantPrepareSubmit
          , trassVariantValidate
          , trassVariantInstall
          , trassVariantScript
          ]
        stop
        wait ContainerStopped (-1)
        destroy
        return code

