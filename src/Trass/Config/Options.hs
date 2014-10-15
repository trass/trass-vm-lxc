module Trass.Config.Options where

import Control.Applicative

import Data.Aeson
import Data.Monoid
import Data.Text (Text)

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HashMap

import Trass.Config.Util

type OptionKey   = Text
type OptionValue = TextValue
type Options m   = Map OptionKey (Map OptionValue (Configuration m))

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

