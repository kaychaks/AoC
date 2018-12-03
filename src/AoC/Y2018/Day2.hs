-- |

{-# LANGUAGE TupleSections #-}

module AoC.Y2018.Day2
where

import Papa
import qualified Data.Map as M (fromListWith, toList)
import qualified Data.Text as T (lines, unpack)
import qualified Data.Text.IO as T (readFile)
import qualified Data.Sequence as C (fromList, elemIndicesL,deleteAt)

type BoxIDs = [String]
data IDMeta = IDMeta
  {
      _id :: String
    , _numTwice :: Int
    , _numThrice :: Int
  } deriving Show

type IDDB = [IDMeta]

parseInput :: FilePath -> IO BoxIDs
parseInput f = do
 ls <- T.readFile f
 return $ T.unpack <$> T.lines ls

transformToIDMeta :: String -> IDMeta
transformToIDMeta s = mkMeta (filter (> 1) $ snd <$> M.toList (M.fromListWith (+) $ (, 1)  <$> s)) where
  f :: (Int -> Bool) -> [Int] -> Int
  f p xs = maybe 0 (const 1) (find p xs)
  mkMeta :: [Int] -> IDMeta
  mkMeta xs = IDMeta s (f (==2) xs) (f (==3) xs)

totalTwiceCounts :: IDDB -> Int
totalTwiceCounts = sum . (_numTwice <$>)
  
totalThriceCounts :: IDDB -> Int
totalThriceCounts = sum . (_numThrice <$>)

checksum :: BoxIDs -> Int
checksum is = let db = transformToIDMeta <$> is
                  in
                liftA2 (*) totalTwiceCounts totalThriceCounts db

doDay2Puzzle1 :: FilePath -> IO Int
doDay2Puzzle1 = (<$>) checksum . parseInput

eqIndex :: String -> String -> Maybe Int
eqIndex a b = let is = C.elemIndicesL False $ C.fromList $ zipWith (==) a b
                  in
                case is of
                  [x] -> Just x
                  _ -> Nothing

commonLetters :: [String] -> Maybe String
commonLetters xs = let cs = take 1 $ do
                         a <- xs
                         b <- xs
                         case eqIndex a b of
                           Just i -> [toList $ C.deleteAt i (C.fromList a)]
                           Nothing -> []
                       in
                     case cs of
                       [x] -> Just x
                       _ -> Nothing

doDay2Puzzle2 :: FilePath -> IO (Maybe String)
doDay2Puzzle2 = (<$>) commonLetters . parseInput