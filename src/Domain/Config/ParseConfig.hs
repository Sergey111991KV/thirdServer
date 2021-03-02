module Domain.Config.ParseConfig where

import           ClassyPrelude                  ( ($)
                                                , Monad(return)
                                                , Text
                                                , String
                                                , Alternative((<|>))
                                                )
import qualified Text.Parsec                   as Parsec

type ConfigPair = (String, String)

toPairs :: Parsec.Parsec Text () ConfigPair
toPairs = do
  key <- Parsec.many1 (Parsec.letter <|> Parsec.digit <|> Parsec.char ':')
  Parsec.spaces
  value <-
    Parsec.many1 (Parsec.letter <|> Parsec.digit <|> Parsec.char ':')
      <|> optionPostgresParser
  return (key, value)

configParser :: Parsec.Parsec Text () [ConfigPair]
configParser = Parsec.many1 $ do
  pair <- toPairs
  Parsec.eof <|> eolParser
  return pair

eolParser :: Parsec.Parsec Text () ()
eolParser = do
  _ <- Parsec.endOfLine
  return ()

optionPostgresParser :: Parsec.Parsec Text () String
optionPostgresParser = do
  _     <- Parsec.char '"'
  value <- Parsec.many1
    (Parsec.letter <|> Parsec.digit <|> Parsec.space <|> Parsec.oneOf "'=")
  _ <- Parsec.char '"'
  return value
