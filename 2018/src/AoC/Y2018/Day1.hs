module AoC.Y2018.Day1(
  makeFreqChanges
  , calcFreq
  , doDay1Puzzle1
  , checkRepeating
  , doDay1Puzzle2
) where

import Papa
import qualified Data.Set as S (Set,empty, insert, member)
import qualified Data.Text as T (lines)
import qualified Data.Text.IO as T (readFile)
import qualified Data.Text.Read as T (signed, decimal)

type FreqChanges = [Int]

makeFreqChanges :: FilePath -> IO FreqChanges
makeFreqChanges  file = do
  input <- T.readFile file
  let ls = T.lines input
  return $ either (const 0) fst . T.signed T.decimal <$> ls
  
calcFreq :: FreqChanges -> Int
calcFreq = sum

hasDuplicates :: S.Set Int -> Maybe (NonEmpty Int) -> Maybe Int
hasDuplicates _ Nothing  = Nothing
hasDuplicates s (Just (x :| xs)) = if x `S.member` s then Just x else hasDuplicates (x `S.insert` s) (nonEmpty xs)

checkRepeating :: FreqChanges -> Maybe Int
checkRepeating  = hasDuplicates S.empty . Just . scanl (+) 0 . cycle
                      

doDay1Puzzle1 :: FilePath -> IO Int
doDay1Puzzle1 = (<$>) calcFreq  . makeFreqChanges

doDay1Puzzle2 :: FilePath -> IO (Maybe Int)
doDay1Puzzle2 = (<$>) checkRepeating . makeFreqChanges
