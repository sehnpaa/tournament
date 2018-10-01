module Parse where

import Types

import Control.Monad (void)
import Data.Text
import qualified Text.Parsec as P
import qualified Text.Parsec.Text as PT

import Data.Monoid

allowedScores :: [Int]
allowedScores = [0..5]

parseMatchups :: Text -> Either P.ParseError [Matchup]
parseMatchups = P.parse matchups ""

matchups :: PT.Parser [Matchup]
matchups = P.many $ matchup <* P.many P.endOfLine

matchup :: PT.Parser Matchup
matchup = do
    first <- Parse.name
    void nameSeparator
    second <- Parse.name
    void $ P.string " => "
    score1 <- score
    void nameSeparator
    score2 <- score
    return $ Matchup first second score1 score2

nameSeparator :: PT.Parser ()
nameSeparator = do
    void $ P.many $ P.char ' '
    void $ P.char '-'
    void $ P.many $ P.char ' '
    return ()

name :: PT.Parser String
name = P.many $ P.noneOf " "

score :: PT.Parser Int
score = do
    s <- P.oneOf $ Prelude.concatMap show allowedScores
    return $ read [s]