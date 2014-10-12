{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Trass where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Yaml

import Control.Applicative
import Control.Monad.IO.Class
import Control.Concurrent

import System.LXC
import System.Exit
import System.Process
import System.Unix.Directory (withTemporaryDirectory)
import System.Posix.Files
import System.Posix.IO (handleToFd)

data TrassConfig = TrassConfig
  { trassConfigDownload         :: String
  , trassConfigEnv              :: [String]
  , trassConfigPrepareContainer :: [String]
  , trassConfigPrepareSubmit    :: [String]
  , trassConfigValidate         :: [String]
  , trassConfigInstall          :: [String]
  , trassConfigScript           :: [String]
  }
  deriving (Eq, Show)

instance FromJSON TrassConfig where
   parseJSON (Object v) = TrassConfig
                      <$> v .: "download"
                      <*> v .: "env"
                      <*> v .: "prepare_container"
                      <*> v .: "prepare_submit"
                      <*> v .: "validate"
                      <*> v .: "install"
                      <*> v .: "script"
   parseJSON _          = empty

instance ToJSON TrassConfig where
  toJSON TrassConfig{..} = object
    [ "download"          .= trassConfigDownload
    , "env"               .= trassConfigEnv
    , "prepare_container" .= trassConfigPrepareContainer
    , "prepare_submit"    .= trassConfigPrepareSubmit
    , "validate"          .= trassConfigValidate
    , "install"           .= trassConfigInstall
    , "script"            .= trassConfigScript
    ]

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

prepareContainer :: String -> TrassConfig -> FilePath -> FilePath -> IO (Maybe Container)
prepareContainer name TrassConfig{..} hostPath containerPath = do
  let c = Container name Nothing
  withContainer c $ do
    res <- create "download" Nothing Nothing [] (words trassConfigDownload)
    if not res
      then return Nothing
      else do
        start False []
        wait ContainerRunning (-1)

        copyToContainer hostPath containerPath

        -- prepare container
        mapM_ (attach' . ("export " ++)) trassConfigEnv
        attachMany trassConfigPrepareContainer

        stop
        wait ContainerStopped (-1)
        return (Just c)

runSubmission :: Container -> TrassConfig -> IO (Maybe ExitCode)
runSubmission c TrassConfig{..} = do
  withTemporaryDirectory "submission." $ \tempdir -> do
    print tempdir
    setFileMode tempdir accessModes
    msc <- withContainer c $ clone Nothing (Just tempdir) [CloneSnapshot] Nothing Nothing Nothing []
    case msc of
      Nothing -> return Nothing
      Just sc -> withContainer sc $ do
        liftIO $ print sc
        start False []
        wait ContainerRunning (-1)
        code <- attachMany $ concat
          [ trassConfigPrepareSubmit
          , trassConfigValidate
          , trassConfigInstall
          , trassConfigScript
          ]
        stop
        wait ContainerStopped (-1)
        destroy
        return code

