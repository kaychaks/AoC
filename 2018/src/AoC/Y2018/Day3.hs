{-# LANGUAGE TemplateHaskell #-}

module AoC.Y2018.Day3 where

import Papa
import qualified Data.Text as T (unpack)
import qualified Data.Text.IO as T (readFile)
import qualified Data.Map.Strict as C

import AoC.Util.Parser
import qualified Text.Megaparsec as P(someTill,parse,endBy1)
import qualified Text.Megaparsec.Char as P(char,eol)
import qualified Text.Megaparsec.Char.Lexer as L(decimal)

newtype CellID = CellID (Integer,Integer) deriving (Eq, Ord, Show)

type ClaimID = Integer

data ClaimEntry = ClaimEntry {
  _claimID :: ClaimID
  , _left :: Integer
  , _top :: Integer
  , _width :: Integer
  , _height :: Integer
  } deriving Show
makeLenses ''ClaimEntry

headPartial :: [Integer] -> Integer
headPartial [x] = x
headPartial _ = 0

claimParser :: Parser [ClaimEntry]
claimParser = some $ do
  _ <- P.char '#'
  i <- integer
  _ <- symbol "@"
  l <- headPartial <$> P.someTill L.decimal (P.char ',')
  t <- headPartial <$> P.someTill L.decimal (symbol ":")
  w <- headPartial <$> P.someTill L.decimal (P.char 'x')
  h <- headPartial <$> P.endBy1 L.decimal P.eol
  return $ ClaimEntry i l t w h

parseClaimsInput' :: Parser [ClaimEntry] -> String -> [ClaimEntry]
parseClaimsInput' p s = let ps = P.parse p "" s
               in
                 case ps of
                   Left _ -> []
                   Right xs -> xs

parseClaimsInput :: FilePath -> IO [ClaimEntry]
parseClaimsInput f = parseClaimsInput' claimParser . T.unpack <$> T.readFile f


claimLoc :: ClaimEntry -> [(CellID, ClaimID)]
claimLoc c = let cs :: [(Integer, Integer)]
                 cs = [(c^.top + j, c^.left + i) | j <- [1..c^.height] , i <- [1..c^.width]]
             in
               (\ccs -> (CellID ccs, c^.claimID)) <$> cs

mkMap :: [[(CellID, ClaimID)]] -> C.Map CellID [ClaimID]
mkMap = C.fromListWith (++) . (<$>) (\(c,ii) -> (c,[ii])) . concat

pristineClaim :: C.Map CellID [ClaimID] -> [ClaimID]
pristineClaim m = let s = nub $ concat $ C.elems $ C.filter ((==1) . length) m
                      ns = nub $ concat $ C.elems $ C.filter ((\l -> l > 1 && l <=2) . length) m
                      is = nub $ intersect s ns
                  in
                    s \\ is

day3Puzzle1 :: FilePath -> IO Int
day3Puzzle1 f = do
  p <- parseClaimsInput f
  return $ C.size $ C.filter ((>1) . length) $ mkMap $ claimLoc <$> p

day3Puzzle2 :: FilePath -> IO (Maybe ClaimID)
day3Puzzle2 f = do
  p <- parseClaimsInput f
  let xs = pristineClaim $ mkMap $ claimLoc <$> p
  return $
    case xs of
      [x] -> Just x
      _ -> Nothing
