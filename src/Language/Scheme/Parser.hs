module Language.Scheme.Parser
    ( SchemeToken (..)
    , parseScheme
    , schemeParser
    , schemeNumber
    , schemeIdentifier
    , schemeList
    ) where

import           Data.Text   (Text, pack)
import           Text.Parsec

data SchemeToken = SchemeIdentifier Text
                 | SchemeNumber Text
                 | SchemeList [SchemeToken]
                 deriving (Show, Eq)

parseScheme :: Text -> Either ParseError [SchemeToken]
parseScheme = parse schemeParser ""

schemeParser :: Parsec Text () [SchemeToken]
schemeParser = many $ try (schemeList <|> try schemeNumber <|> schemeIdentifier) <* spaces

schemeNumber :: Parsec Text () SchemeToken
schemeNumber = SchemeNumber . pack <$> many1 digit

schemeIdentifier :: Parsec Text () SchemeToken
schemeIdentifier = SchemeIdentifier . pack <$> many1 (alphaNum <|> oneOf supportedIdentifierChars)
    where
        supportedIdentifierChars =
            [ '!', '$', '%', '&', '*', '+', '-', '.', '/'
            , ':', '<', '=', '>', '?', '@', '^', '_', '~'
            ]

schemeList :: Parsec Text () SchemeToken
schemeList = (SchemeList <$>) $ char '(' *> spaces *> schemeParser <* spaces <* char ')'
