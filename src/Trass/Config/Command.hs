{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Trass.Config.Command where

import Control.Applicative

import Data.Aeson
import Data.Monoid

import Trass.Config.Util

type Command = TextValue

newtype Commands = Commands
  { getCommands :: [Command]
  } deriving (Show, Read, Monoid, ToJSON)

type EnvVars = Commands

instance FromJSON Commands where
  parseJSON v = Commands <$> (parseJSON v <|> (\c -> [c]) <$> parseJSON v)

