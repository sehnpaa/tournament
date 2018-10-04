module Test where

import Data.Foldable (traverse_)
import Test.QuickCheck

import Main
import Types

genMatchup :: Gen Matchup
genMatchup = Matchup <$> listOf (elements ['a'..'z']) <*> listOf (elements ['a'..'z']) <*> elements [0..10] <*> elements [0..10]

genScoreboard :: Gen Scoreboard
genScoreboard = Main.buildScoreboard <$> listOf genMatchup

getScores :: Scoreboard -> [Int]
getScores = fmap row_score . unRows

getWins:: Scoreboard -> [Int]
getWins = fmap wins . unRows

getLosses :: Scoreboard -> [Int]
getLosses = fmap losses . unRows

sumOfScoreIsZero :: Scoreboard -> Bool
sumOfScoreIsZero scoreboard = sum (getScores scoreboard) == 0

asManyWinsAsLosses :: Scoreboard -> Bool
asManyWinsAsLosses scoreboard = sum (getWins scoreboard) == sum (getLosses scoreboard)

tests :: [Scoreboard -> Bool]
tests = [sumOfScoreIsZero, asManyWinsAsLosses]

runTests :: [Scoreboard -> Bool] -> IO ()
runTests = traverse_ (quickCheck . withMaxSuccess 1000 . forAll genScoreboard)

