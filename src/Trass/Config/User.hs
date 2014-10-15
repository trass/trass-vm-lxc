module Trass.Config.User where

import Control.Applicative

import Data.Aeson
import Data.Monoid
import Data.Text (Text)

import Trass.Config.Command
import Trass.Config.Util

data TrassUserConfig = TrassUserConfig
  { trassUserConfigUsername       :: Maybe Text
  , trassUserConfigHome           :: Maybe FilePath
  , trassUserConfigPrepare        :: Commands
  }
  deriving (Show)

instance Monoid TrassUserConfig where
  mempty = TrassUserConfig Nothing Nothing mempty
  mappend c c' = TrassUserConfig
    (lastOf'    trassUserConfigUsername)
    (lastOf'    trassUserConfigHome)
    (mappendOf' trassUserConfigPrepare)
    where
      lastOf'    = lastOf c c'
      mappendOf' = mappendOf c c'

instance FromJSON TrassUserConfig where
  parseJSON (Object v) = TrassUserConfig
                     <$> v .:? "username"
                     <*> v .:? "home"
                     <*> v .:? "prepare" .!= mempty
  parseJSON _ = empty

instance ToJSON TrassUserConfig where
  toJSON TrassUserConfig{..} = object $ concat
    [ "username"  .=? trassUserConfigUsername
    , "home"      .=? trassUserConfigHome
    , [ "prepare" .=  trassUserConfigPrepare ] ]

