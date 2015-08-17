{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module WeatherTool
    (
    -- * Methods
      requestWeather
    , requestWeatherWithCode
    -- * Types
    -- ** General
    -- *** ApiSetting
    , ApiSetting(..), url
    , defaultSetting
    -- *** CityCode
    , CityCode
    -- *** WeatherResponse
    , WeatherResponse
    -- ** JSON Scheme
    -- *** Weather
    , Weather(..), area, prefecture, city, forecasts
    -- *** Location
    , Location(..), title, publicTime, location
    -- *** Forecast
    , Forecast(..), date, dateLabel, telop
    ) where

import WeatherTool.JsonScheme

import Control.Exception (SomeException(..), try)

-- HTTPライブラリ(Lensでアクセスすることを前提に作られている)。
import Network.Wreq (defaults, param, asJSON, getWith, responseBody)

import Control.Lens (makeLenses) -- 自分で定義したデータ型をLens化する用。
import Control.Lens ((^.)) -- Lensのゲッター。
import Control.Lens ((&), (.~), (%~)) -- Lensのセッター。
import Control.Lens.Prism (_Right) -- LensでEitherにアクセスする用。

import qualified Data.Text as T -- UTF8用。

--import Network.Wreq (Response)
--import Data.Map (Map)

-- -- | JSONをMapで受け取る用
-- type MapResponse = Response (Map T.Text Value)

-- | 地域を示すID。
--   参照: <http://weather.livedoor.com/forecast/rss/primary_area.xml>
type CityCode = T.Text

-- | requestWeatherの返り値。失敗時は例外が入ります。(throwはされません)。
type WeatherResponse = Either SomeException Weather

-- | APIのための設定(現在はURLのみ)
data ApiSetting = ApiSetting {
  _url :: String
} deriving (Show, Eq)
makeLenses ''ApiSetting

-- | デフォルトの設定
defaultSetting :: ApiSetting
defaultSetting = ApiSetting {
  _url = "http://weather.livedoor.com/forecast/webservice/json/v1"
}

-- | APIを呼び、CityCodeで指定された地点の天気を取得します。
--   API呼び出しに失敗した場合はEitherに例外を包んで返します。
requestWeather :: ApiSetting -> CityCode -> IO WeatherResponse
requestWeather setting code = do
  let opts = defaults & param "city" .~ [code]
  r <- try $ asJSON =<< getWith opts (setting ^. url)
  let r_ = r & _Right %~ (^. responseBody)
  return r_

-- | requestWeatherを呼び、CityCode付きで返します。
requestWeatherWithCode :: ApiSetting -> CityCode -> IO (CityCode, WeatherResponse)
requestWeatherWithCode setting code
  = fmap ((,) code) (requestWeather setting code)

