module Render where

import qualified Data.Text as Text
import Text.Layout.Table

import qualified Types as T

griddify :: [[String]] -> [String]
griddify = gridLines
    [ column expand left def def
    , column expand center def def
    , column expand center def def
    , column expand center def def
    , column expand center def def
    ]

render :: T.Scoreboard -> [String]
render = griddify . appendTitles . fmap renderRow  . T.unRows

appendTitles :: [[String]] -> [[String]]
appendTitles = (++) [["Name", "Wins", "Draws", "Losses", "Score"]]

renderRow :: T.Row -> [String]
renderRow (T.Row n w d l s) = [Text.unpack n, show w, show d, show l, show s]