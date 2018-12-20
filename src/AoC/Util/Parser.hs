-- | Parsing stuff

module AoC.Util.Parser where

import Papa
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Time.LocalTime (LocalTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale)

type Parser = P.Parsec Void String

sc :: Parser ()
sc = L.space P.space1 P.empty P.empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

integer :: Parser Integer
integer = lexeme L.decimal

datetime :: Parser (Maybe LocalTime)
datetime = do
  _ <- P.count 1 $ P.char '['
  y <- L.decimal 
  _ <- P.count 1 $ P.char '-'
  m <- L.decimal
  _ <- P.count 1 $ P.char '-'
  d <- L.decimal
  _ <- P.count 1 P.space1
  h <- L.decimal
  _ <- P.count 1 $ P.char ':'
  k <- L.decimal :: Parser Integer
  _ <- P.count 1 $ P.char ']'
  let dt = show y <> "-" <> show m <> "-" <> show d <> " " <> show h <> ":" <> show k
  return $ parseTimeM True defaultTimeLocale "%Y-%-m-%-d %k:%-M" dt
