{-# LANGUAGE TemplateHaskell #-}

module AoC.Y2018.Day4 where

import Prelude (enumFromTo)
import Papa
import qualified Data.Map.Strict as M
import qualified Data.Text.IO as T (readFile)
import qualified Data.Text as T (unpack)
import AoC.Util.Parser
import AoC.Util.Util
import Data.Time.LocalTime
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Control.Monad.Combinators as PC()


type Guard = Integer
type NapMinutes = [Integer]
type NapDuration = Sum Integer
type NapTimeFreq = [(Integer, [Integer])] -- TODO: maybe an IntMap

type NapEvent = (Guard, NapMinutes, NapDuration) -- TODO: to a datatype
type NapEvent' = (Guard, (NapTimeFreq, NapDuration)) -- TODO: maybe a map

data Record = AtWork Guard LocalTime
            | Awake LocalTime
            | Asleep LocalTime
            deriving (Show)

makePrisms ''Record

dateParser :: Parser (Maybe LocalTime)
dateParser = (lexeme . P.try) datetime

atWorkParser :: Parser Record
atWorkParser = dateParser >>= \t -> do
  symbol "Guard"
  P.char '#'
  i <- integer
  symbol "begins"
  P.string "shift"
  P.eol
  return $ AtWork i (timeSansMaybe t)

awakeParser :: Parser Record
awakeParser = dateParser >>= \t -> do
  symbol "wakes"
  P.string "up"
  P.eol
  return $ Awake (timeSansMaybe t)

asleepParser :: Parser Record
asleepParser = dateParser >>= \t -> do
  symbol "falls"
  P.string "asleep"
  P.eol
  return $ Asleep (timeSansMaybe t)

recParser :: Parser [Record]
recParser = P.some $
            P.try atWorkParser
            <|> P.try asleepParser
            <|> P.try awakeParser

parseRecords' :: FilePath -> IO [Record]
parseRecords' f = g . P.parse recParser "" . T.unpack <$> T.readFile f
  where
    sortFn :: Record -> LocalTime
    sortFn (AtWork _ t) = t
    sortFn (Asleep t) = t
    sortFn (Awake t) = t
    g e = case e of
      Left _ -> []
      Right xs -> sortOn sortFn xs

processRecords :: [Record] -> [NapEvent]
processRecords = foldl' g [] 
  where
    calcNapMins :: LocalTime -> [Integer] -> [Integer]
    calcNapMins _ [] = []
    calcNapMins t (x : xs) = [x .. (minute t - 1)] ++ xs

    napDuration :: Sum Integer -> [Integer] -> Sum Integer
    napDuration t xs =  fromMaybe 0 $ (\a -> subtract a <$> t) <$> firstOf traverse xs

    g :: [NapEvent] -> Record -> [NapEvent]
    g m (AtWork gid _) = (gid, [], mempty) : m
    g m (Asleep t) = m & ix 0 %~ (_2 %~ (minute t :))
    g m (Awake t) = m & ix 0 -- only do the following for the head element, if present
                      %~
                      (\i ->
                          i & _3               -- update 3rd element of the tuple i.e. sum
                              <>~ napDuration (pure $ minute t) (i ^. _2))

                      . (_2  %~ calcNapMins t) -- update 2nd element of the tuple i.e. array

groupRecords :: [NapEvent] -> [NapEvent']
groupRecords =  M.toList
              . M.map
                  ( _1 %~
                      (M.toDescList
                      . invertMap
                      . freq)
                  )
              . M.fromListWith (\(b,c) (d,e) -> (b ++ d, c + e))
              . over mapped (\(a,b,c) -> (a, (b,c)))
              . toListOf (folded.folded)
              . groupBy ((==) `on` fst)
              . sortBy (comparing fst)

mkResult :: Traversable t => t NapEvent' -> Maybe (Guard, Integer)
mkResult = liftA2 (\ma mb -> do a <- ma; b <- mb; pure (a,b))
                   (preview (traverse . _1))
                   (preview (traverse . _2 . _1 . ix 0 . _2 . ix 0))

mostAsleep :: [NapEvent'] -> Maybe (Guard, Integer)
mostAsleep = mkResult . sortBy (comparing (Down . snd . snd))

mostFreqAsleep :: [NapEvent'] -> Maybe (Guard, Integer)
mostFreqAsleep = mkResult
                  . maximumByOf traverse (compare `on` (\a -> a ^? _2 . _1 . traverse . _1))

puzzleBootStrap :: ([NapEvent'] -> Maybe (Guard, Integer)) -> FilePath -> IO (Maybe Integer)
puzzleBootStrap f p = fmap (uncurry (*))  . f . groupRecords  . processRecords <$> parseRecords' p

day4Puzzle1 :: FilePath -> IO (Maybe Integer)
day4Puzzle1 = puzzleBootStrap mostAsleep 

day4Puzzle2 :: FilePath -> IO (Maybe Integer)
day4Puzzle2  = puzzleBootStrap mostFreqAsleep