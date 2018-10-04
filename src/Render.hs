module Render where

import qualified Data.Text as Text
import Text.Layout.Table

import qualified Types as T

instance Show T.Scoreboard where
    show = render

table :: [String] -> [[String]] -> String
table titles = tableString
    [ column expand left def def
    , column expand center def def
    , column expand center def def
    , column expand center def def
    , column expand center def def ] unicodeRoundS (titlesH titles) . fmap rowG

render :: T.Scoreboard -> String
render = table titles . fmap renderRow . T.unRows

titles :: [String]
titles = ["Name", "Wins", "Draws", "Losses", "Score"]

renderRow :: T.Row -> [String]
renderRow (T.Row n w d l s) = [Text.unpack n, show w, show d, show l, show s]