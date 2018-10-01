module Types where

import Data.Text

data Matchup = Matchup
    { first :: String
    , second :: String
    , firstScore :: Int
    , secondScore :: Int }

data MatchupResult = WL WinLoss | D Draw

data WinLoss = WinLoss
    { win :: Res
    , loss :: Res }

data Draw = Draw
    { draw_first :: String
    , draw_second :: String
    , draw_score :: Int }

data Res = Res { res_name :: String, res_score :: Int}

data Row = Row
    { name :: Text
    , wins :: Int
    , draws :: Int
    , losses :: Int
    , row_score :: Int}

newtype Scoreboard = Scoreboard { unRows :: [Row] }