module Language.Scheme.Parser
    ( SchemeToken (..)
    , parseScheme
    , schemeParser
    , schemeNumber
    , schemeIdentifier
    , schemeList
    , schemeQuote
    , schemeString
    , schemeComment
    ) where

import           Control.Monad (void)
import           Data.Text     (Text, pack)
import           Text.Parsec

data SchemeToken = SchemeIdentifier Text
                 | SchemeNumber Text
                 | SchemeList [SchemeToken]
                 | SchemeQuote SchemeToken
                 | SchemeString Text
                 | SchemeComment Text
                 deriving (Show, Eq)

parseScheme :: Text -> Either ParseError [SchemeToken]
parseScheme = parse (schemeParser <* eof) ""

schemeParser :: Parsec Text () [SchemeToken]
schemeParser =
    many $ (try schemeList <|> try schemeNumber <|> try schemeIdentifier <|> try schemeQuote <|> try schemeString <|> schemeComment) <* spaces

schemeNumber :: Parsec Text () SchemeToken
schemeNumber = SchemeNumber . pack <$> many1 digit

schemeIdentifier :: Parsec Text () SchemeToken
schemeIdentifier = SchemeIdentifier . pack <$> many1 (alphaNum <|> oneOf supportedIdentifierChars)
    where
        supportedIdentifierChars = "!$%&*+-./:<=>?@^_~"

schemeList :: Parsec Text () SchemeToken
schemeList = (SchemeList <$>) $ char '(' *> spaces *> schemeParser <* spaces <* char ')'

schemeQuote :: Parsec Text () SchemeToken
schemeQuote =
    (SchemeQuote <$>) $ char '\'' *> spaces *> (try schemeList <|> try schemeNumber <|> try schemeIdentifier <|> try schemeQuote <|> schemeString)

schemeString :: Parsec Text () SchemeToken
schemeString = (SchemeString . pack <$>) $ char '"' *> manyTill anyChar (char '"')

schemeComment :: Parsec Text () SchemeToken
schemeComment = (SchemeComment . pack <$>) $ char ';' *> manyTill anyChar (try $ eof <|> void newline)
