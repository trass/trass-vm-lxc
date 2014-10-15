module Trass.Config.Submission where

import Control.Applicative

import Data.Aeson
import Data.Monoid
import Data.Text (Text)

import Trass.Config.Command
import Trass.Config.Util

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

