{-# LANGUAGE OverloadedStrings #-}

module Main where

import WeatherTool

import Control.Exception (SomeException)

-- 今回、リソース管理用のモナドResourceTを使う。
-- runResourceTはResourceTによるリソース管理を動かすために必要。
import Control.Monad.Trans.Resource (runResourceT)

-- Haskell標準のLazyなIOはリソースの解放タイミングを保証できない。
-- Conduitはそのあたりの問題を解決するライブラリ。
-- それに加えて大量のデータストリームを捌くことも考えて作られている。
import Data.Conduit (($$), (=$=))
import qualified Data.Conduit.List as C
import qualified Data.Conduit.Binary as C
import qualified Data.Conduit.Text as C

-- UTF8文字列用。
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as T
import qualified Data.Text.Format as T

import Control.Monad.IO.Class (liftIO) -- ちょっとした型合わせに使う。
import Control.Lens ((^.)) -- Lensのゲッター。

-- | 今回投げるCityCode。
cityCodes = ["020010", "050010", "999999", "030010", "060010", "040010", "070010"]

-- | 出力ファイル名。
outputFileName = "tmp.txt"

-- | Weatherの出力形式を決めます。
formatWeather :: Weather -> T.Text
formatWeather w = T.unlines $ (w ^. title) : (map formatForecast (w ^. forecasts))
  where formatForecast f = TL.toStrict $ T.format "\t{} {} {}" [f ^. dateLabel, f ^. date, f ^. telop]

-- | WeatherResponseの出力形式を決めます。
formatResponse :: WeatherResponse -> T.Text
formatResponse (Left _) = "*** Failure. ***\n"
formatResponse (Right w) = formatWeather w

-- | requestWeatherWithCodeの返り値の出力形式を決めます。
format :: (CityCode, WeatherResponse) -> T.Text
format (code, response) = T.concat [code, ": ", formatResponse response, "\n"]

-- | main関数。APIを何度か呼んで結果をファイルに出力する。
main :: IO ()
main =
  runResourceT
  $ C.sourceList cityCodes -- CityCodeのリストをConduitのSourceにする。
  =$= C.mapM (liftIO . requestWeatherWithCode defaultSetting) -- APIの呼び出し。
  =$= C.map format -- APIの結果を出力用にフォーマット。
  =$= C.encode C.utf8 -- Text型をバイト列にエンコード。
  $$ C.sinkFile outputFileName -- ファイルをConduitのSinkにする。

