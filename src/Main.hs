{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Parallel.Strategies
import Data.List as L
import Data.Text
import Data.Text.IO as TextIO

import Parse (parseMatchups)
import Types

main :: IO ()
main = TextIO.readFile "matchups.txt" >>= printResult

printResult :: Text -> IO ()
printResult content = case fmap (sortByWins . buildScoreboard) (parseMatchups content) of
    Right c -> print c
    Left x -> Prelude.putStrLn $ "Error: " ++ show x

sortByWins :: Scoreboard -> Scoreboard
sortByWins = Scoreboard . sortBy (\(Row _ w1 _ _ _) (Row _ w2 _ _ _) -> compare w2 w1) . unRows

instance Show Scoreboard where
    show (Scoreboard rows) = let firstRow = "Name:\tWins:\tDraws:\tLosses:\tScore:"
        in firstRow ++ "\n" ++ L.intercalate "\n" (fmap show rows)

instance Semigroup Scoreboard where
    xs <> Scoreboard [] = xs
    Scoreboard [] <> ys = ys
    Scoreboard (x:xs) <> ys =
        Scoreboard xs <> Scoreboard (upsert (\a b -> name a == name b) x (unRows ys))

upsert :: Semigroup a => (a -> a -> Bool) -> a -> [a] -> [a]
upsert _ a [] = [a]
upsert isMatch x (y:ys) =
    if isMatch x y
        then x <> y : ys
        else y : upsert isMatch x ys

instance Monoid Scoreboard where
    mempty = Scoreboard []

instance Semigroup Row where
    Row n1 w1 d1 l1 s1 <> Row n2 w2 d2 l2 s2 = Row n1 (w1 + w2) (d1 + d2) (l1 + l2) (s1 + s2)

instance Show Row where
    show (Row n w d l s) = Data.Text.unpack n ++ "\t" ++ show w ++ "\t" ++ show d ++ "\t" ++ show l ++ "\t" ++ show s

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