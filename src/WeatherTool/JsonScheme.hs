{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module WeatherTool.JsonScheme where

import Control.Monad (mzero)
import Control.Applicative ((<$>), (<*>))

import qualified Data.Text as T
import Control.Lens (makeLenses)
import Data.Aeson (FromJSON, parseJSON, (.:), Value(Object))

-- JSONをパースするためのデータ型を定義する(先頭の_はLensのための書き方)
data Location = Location {
    _area :: T.Text
  , _prefecture :: T.Text
  , _city :: T.Text
} deriving (Eq, Show)
-- これでJSONからHaskellのデータ型への変換ができるようになる
instance FromJSON Location where
  parseJSON (Object v) =
    Location <$> v .: "area"
      <*> v .: "prefecture"
      <*> v .: "city"
  parseJSON _ = mzero
-- これでLensによるデータ型へのアクセスができるようになる
makeLenses ''Location

data Forecast = Forecast {
    _date :: T.Text
  , _dateLabel :: T.Text
  , _telop :: T.Text
} deriving (Eq, Show)
instance FromJSON Forecast where
  parseJSON (Object v) =
    Forecast <$> v .: "date"
      <*> v .: "dateLabel"
      <*> v .: "telop"
  parseJSON _ = mzero
makeLenses ''Forecast

-- | 天気APIの返り値JSONの仕様です。
--   参照: <http://weather.livedoor.com/weather_hacks/webservice>
data Weather = Weather {
    _title :: T.Text
  , _publicTime :: T.Text
  , _location :: Location
  , _forecasts :: [Forecast]
} deriving (Eq, Show)
instance FromJSON Weather where
  parseJSON (Object v) =
    Weather <$> v .: "title"
      <*> v .: "publicTime"
      <*> v .: "location"
      <*> v .: "forecasts"
  parseJSON _ = mzero
makeLenses ''Weather

