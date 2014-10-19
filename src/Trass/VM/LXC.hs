module Trass.VM.LXC where

import Control.Applicative
import Control.Monad.IO.Class

import qualified Data.Text as Text

import System.Exit
import System.FilePath
import System.Process
import System.Unix.Directory (withTemporaryDirectory)
import System.Posix.Files
import System.Posix.IO (handleToFd)
import qualified System.LXC as LXC

import Trass.VM
import Trass.VM.Config
import Trass.Config.Util

instance MonadVM LXC.LXC where
  data VM LXC.LXC = VmLxc { getVmLxc :: LXC.Container }

  withVM = LXC.withContainer . getVmLxc

  create path TrassConfig{..} = do
    b <- LXC.withContainer c $ do
      case (,,) <$> trassConfigDist <*> trassConfigRelease <*> trassConfigArch of
        Nothing -> return False
        Just (dist, release, arch) -> do
          LXC.create "download" Nothing Nothing [] $ map Text.unpack ["-d", dist, "-r", release, "-a", arch]
    return $ if b then Just (VmLxc c) else Nothing
    where
      (lxcpath, name) = splitFileName path
      c = LXC.Container name (Just lxcpath)

  clone path vm = fmap VmLxc <$> do
    LXC.withContainer (getVmLxc vm) $ do
      LXC.clone (Just name) (Just lxcpath) [LXC.CloneSnapshot] Nothing Nothing Nothing []
    where
      (lxcpath, name) = splitFileName path

  start = do
    LXC.start False []
    LXC.wait LXC.ContainerRunning (-1)

  stop = do
    LXC.stop
    LXC.wait LXC.ContainerStopped (-1)

  destroy = LXC.destroy

  sendFile from to = hostToLXC
    (proc "cat" [from])
    (TextValue . Text.pack $ "cat >" ++ to)

  sendDirectory from to = hostToLXC
    (proc "tar" ["-p", "-C", from, "-zcf", "-", "."])
    (TextValue . Text.pack $ "mkdir -p " ++ show to ++ " && tar -p -C " ++ show to ++ " -zxf -")

  execute env user wd cmd = do
    case wd of
      Nothing   -> return ()
      Just path -> do
        exec $ "cd " ++ show path
        return ()
    exec $ Text.unpack $ getTextValue cmd
    where
      run = LXC.attachRunWait LXC.defaultAttachOptions { LXC.attachExtraEnvVars = env }
      exec sh = do
        liftIO $ putStrLn $ maybe "# " (const "$ ") user ++ sh
        case user of
          Nothing   -> run "sh" [ "sh", "-c", sh ]
          Just name -> run "su" [ "su", "-m", "-l", name, "-c", sh ]

hostToLXC :: CreateProcess -> Command -> LXC.LXC (Maybe ExitCode)
hostToLXC hostProc lxcCmd = do
  outFd <- liftIO $ do
    (_, Just hout, _, _) <- createProcess hostProc { std_out = CreatePipe }
    handleToFd hout
  LXC.attachRunWait
    LXC.defaultAttachOptions { LXC.attachStdinFD = outFd }
    "sh" ["sh", "-c", Text.unpack (getTextValue lxcCmd) ]

