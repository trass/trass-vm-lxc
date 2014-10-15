module Trass.Config (
  module Trass.Config.Command,
  module Trass.Config.Submission,
  module Trass.Config.User,
  module Trass.Config.Util,
  TrassConfig(..),
) where

import Control.Applicative

import Data.Aeson
import Data.Monoid
import Data.Text (Text)

import Trass.Config.Command
import Trass.Config.Submission
import Trass.Config.User
import Trass.Config.Util

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

