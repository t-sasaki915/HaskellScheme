module Language.Scheme.Parser
    ( SchemeExpr (..)
    , parseScheme
    , schemeParser
    ) where

import           Control.Monad (void)
import           Data.Text     (Text, pack)
import           Text.Parsec

data SchemeExpr = Evaluation [SchemeExpr]
                | Reference Text
                deriving Show

parseScheme :: Text -> Either ParseError [SchemeExpr]
parseScheme = parse schemeParser ""

schemeParser :: Parsec Text () [SchemeExpr]
schemeParser = expressions

expressions :: Parsec Text () [SchemeExpr]
expressions = many1 $ do
    expr <- try expression <|> identifier
    spaces
    return expr

expression :: Parsec Text () SchemeExpr
expression = do
    void $ char '('
    spaces

    inside <- expressions

    spaces
    void $ char ')'

    return (Evaluation inside)

identifier :: Parsec Text () SchemeExpr
identifier = Reference . pack <$> many1 (alphaNum <|> oneOf supportedIdentifierCharacters)
    where
        supportedIdentifierCharacters =
            [ '!', '$', '%', '&', '*', '+', '-', '.', '/'
            , ':', '<', '=', '>', '?', '@', '^', '_', '~'
            ]
