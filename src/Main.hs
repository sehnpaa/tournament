{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Foldable (traverse_)
import Control.Parallel.Strategies
import Data.List as L
import Data.Ord (comparing)
import Data.Text
import Data.Text.IO as TextIO

import Helper (upsert)
import Parse (parseMatchups)
import Render (render)
import Types

main :: IO ()
main = TextIO.readFile "matchups.txt" >>= printResult

printResult :: Text -> IO ()
printResult content = case render . sortScoreboard . buildScoreboard <$> parseMatchups content of
    Right c -> traverse_ Prelude.putStrLn c
    Left x -> Prelude.putStrLn $ "Error: " ++ show x

sortScoreboard :: Scoreboard -> Scoreboard
sortScoreboard = Scoreboard . sortBy (mconcat rowOrderings) . unRows

-- flip changes the desc to asc and asc to desc
rowOrderings :: [Row -> Row -> Ordering]
rowOrderings = [flip $ comparing wins, flip $ comparing draws, comparing losses, flip $ comparing row_score]

instance Semigroup Scoreboard where
    xs <> Scoreboard [] = xs
    Scoreboard [] <> ys = ys
    Scoreboard (x:xs) <> ys =
        Scoreboard xs <> Scoreboard (upsert (\a b -> name a == name b) x (unRows ys))

instance Monoid Scoreboard where
    mempty = Scoreboard []

instance Semigroup Row where
    Row n1 w1 d1 l1 s1 <> Row n2 w2 d2 l2 s2 = Row n1 (w1 + w2) (d1 + d2) (l1 + l2) (s1 + s2)

getScoreboard :: MatchupResult -> Scoreboard
getScoreboard (WL (WinLoss (Res wn ws) (Res ln ls))) = Scoreboard
    [ Row (Data.Text.pack wn) 1 0 0 (ws-ls)
    , Row (Data.Text.pack ln) 0 0 1 (ls-ws)]
getScoreboard (D (Draw w l s)) = Scoreboard
    [ Row (Data.Text.pack w) 0 1 0 s
    , Row (Data.Text.pack l) 0 1 0 s]

buildScoreboard :: [Matchup] -> Scoreboard
buildScoreboard ms = mconcat (L.map (getScoreboard . getMatchupResult) ms `using` parList rseq)

getMatchupResult :: Matchup -> MatchupResult
getMatchupResult (Matchup n1 n2 s1 s2) = case compare s1 s2 of
    GT -> WL $ WinLoss (Res n1 s1) (Res n2 s2)
    EQ -> D $ Draw n1 n2 s1
    LT -> WL $ WinLoss (Res n2 s2) (Res n1 s1)