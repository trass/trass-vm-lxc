module Trass where

import Data.List
import Data.Monoid
import Data.Maybe

import Data.Text (Text)
import qualified Data.Text as Text

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import System.LXC
import System.Exit
import System.FilePath
import System.Process
import System.Unix.Directory (withTemporaryDirectory)
import System.Posix.Files
import System.Posix.IO (handleToFd)

import Trass.Config

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

submit :: Container -> FilePath -> [FilePath] -> TrassConfig -> IO (Maybe ExitCode)
submit base submitFile taskDirs TrassConfig{..} = do
  let TrassSubmissionConfig{..} = trassConfigSubmission
  withTemporaryDirectory "submission." $ \tempdir -> do
    setFileMode tempdir accessModes
    mc <- withContainer base $ clone Nothing (Just tempdir) [CloneSnapshot] Nothing Nothing Nothing []
    case mc of
      Nothing   -> return Nothing
      Just temp -> withContainer temp $ do
        start False []
        wait ContainerRunning (-1)

        let homeDir     = fromMaybe "" $ trassUserConfigHome trassConfigUser
            taskDir'    = fromMaybe "trass_task_dir"    trassConfigTaskDir
            submitFile' = fromMaybe "trass_submit_file" trassSubmissionConfigFile

        -- merge and copy multiple task directories
        forM_ taskDirs $ \taskDir -> do
          dirToDir taskDir $ homeDir </> taskDir'
        -- copy submitted file
        fileToFile submitFile $ homeDir </> taskDir' </> submitFile'

        -- set environment
        let env  = getCommands trassConfigEnvironment <> [TextValue $ "USER=" <> fromMaybe "root" (trassUserConfigUsername trassConfigUser)]
            env' = map (Text.unpack . getTextValue) env
            attachMany' = attachMany env' trassConfigUser { trassUserConfigHome = (</> taskDir') <$> trassUserConfigHome trassConfigUser }

        -- run commands
        mcode <- attachMany' . mconcat . map (fromMaybe mempty) $
                    [ trassSubmissionConfigBeforeInstall
                    , trassSubmissionConfigInstall
                    , trassSubmissionConfigBeforeScript
                    , trassSubmissionConfigScript ]

        -- run after* commands
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

