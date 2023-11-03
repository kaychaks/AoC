{-# LANGUAGE TupleSections #-}

module AoC.Util.Util where

import Papa
import Data.Time.LocalTime
import qualified Data.Map as M
import Data.Time.Calendar

minute :: LocalTime -> Integer
minute = fromIntegral . todMin . localTimeOfDay

freq :: [Integer] -> M.Map Integer Integer
freq = M.fromListWith (+) . (<$>) (,1)

bogusTime :: LocalTime
bogusTime = LocalTime (fromGregorian 1500 1 1) midnight

timeSansMaybe :: Maybe LocalTime -> LocalTime
timeSansMaybe = fromMaybe bogusTime

invertMap :: (Ord a , Ord b) => M.Map a b -> M.Map b [a]
invertMap = M.fromListWith (++) . M.foldMapWithKey (\ k a -> [(a, [k])])