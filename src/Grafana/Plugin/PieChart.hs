{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Grafana.Plugin.PieChart where

import Data.Aeson (ToJSON(..))
import Data.Aeson ((.=), Value(..))
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Aeson.Key (fromText)

import qualified Data.Aeson as AE

import Grafana

data PieType = Donut | Pie

instance ToJSON PieType where
  toJSON = \case
    Donut -> "donut"
    Pie -> "pie"

data PieChart = PieChart
  { pieChartTitle :: Text
  , pieChartQueries :: [GraphiteQuery]
  , pieChartUnit :: Maybe UnitFormat
  , pieChartAliasColors :: Map Text RGBA
  , pieType :: PieType
  }

pieChartToPairs :: PieChart -> [(AE.Key, AE.Value)]
pieChartToPairs (PieChart {..}) =
  [ fromText "type" .= String "grafana-piechart-panel"
  , fromText "title" .= pieChartTitle
  , fromText "targets" .= makeTargets pieChartQueries
  , fromText "format" .= pieChartUnit
  , fromText "pieType" .= pieType
  , fromText "aliasColors" .= pieChartAliasColors
  ]

pieChartPanel :: PieChart -> GridPos -> Panel
pieChartPanel = Panel . pieChartToPairs
