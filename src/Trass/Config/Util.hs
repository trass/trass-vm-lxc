{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Trass.Config.Util where

import Control.Applicative

import Data.Aeson
import Data.Aeson.Types
import Data.Monoid

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HashMap

newtype TextValue = TextValue
  { getTextValue :: Text
  } deriving (Eq, Ord, Show, Read, Monoid, ToJSON)

instance FromJSON TextValue where
  parseJSON (String s) = pure $ TextValue s
  parseJSON (Number n) = pure $ TextValue (Text.pack $ show n)
  parseJSON (Bool b)   = pure $ TextValue (if b then "true" else "false")
  parseJSON Null       = pure $ TextValue "null"
  parseJSON v          = pure $ TextValue (read . show $ encode v)

instance FromJSON v => FromJSON (Map TextValue v) where
  parseJSON v = Map.mapKeys TextValue <$> parseJSON v

instance ToJSON v => ToJSON (Map TextValue v) where
  toJSON = toJSON . Map.mapKeys getTextValue

(.=?) :: ToJSON a => Text -> Maybe a -> [Pair]
k .=? v = maybe [] (\v' -> [k .= v']) v

objectUnion :: Value -> Value -> Value
objectUnion (Object v) (Object v') = Object $ HashMap.union v v'
objectUnion _ _ = object []

lastOf :: a -> a -> (a -> Maybe b) -> Maybe b
lastOf x y f = f y <|> f x

mappendOf :: Monoid m => a -> a -> (a -> m) -> m
mappendOf x y f = f x <> f y

