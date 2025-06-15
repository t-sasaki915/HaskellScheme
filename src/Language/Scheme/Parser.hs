module Language.Scheme.Parser
    ( SchemeToken (..)
    , SchemeCharacterType (..)
    , parseScheme
    , schemeParser
    , schemeNumber
    , schemeIdentifier
    , schemeList
    , schemeQuote
    , schemeString
    , schemeComment
    , schemeCharacter
    ) where

import           Control.Monad (void)
import           Data.Functor  (($>))
import           Data.Text     (Text, pack)
import           Text.Parsec

data SchemeToken = SchemeIdentifier Text
                 | SchemeNumber Text
                 | SchemeList [SchemeToken]
                 | SchemeQuote SchemeToken
                 | SchemeString Text
                 | SchemeComment Text
                 | SchemeCharacter SchemeCharacterType
                 deriving (Show, Eq)

data SchemeCharacterType = SimpleCharacter Char
                         | BackspaceCharacter
                         | TabCharacter
                         | NewlineCharacter
                         | LinefeedCharacter
                         | PageCharacter
                         | ReturnCharacter
                         | SpaceCharacter
                         | RuboutCharacter
                         deriving (Show, Eq)

parseScheme :: Text -> Either ParseError [SchemeToken]
parseScheme = parse (schemeParser <* eof) ""

schemeParser :: Parsec Text () [SchemeToken]
schemeParser =
    many $ (try schemeList <|> try schemeNumber <|> try schemeIdentifier <|> try schemeQuote <|> try schemeString <|> try schemeCharacter <|> schemeComment) <* spaces

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
    (SchemeQuote <$>) $ char '\'' *> spaces *>
        (try schemeList <|> try schemeNumber <|> try schemeIdentifier <|> try schemeQuote <|> try schemeString <|> schemeCharacter)

schemeString :: Parsec Text () SchemeToken
schemeString = (SchemeString . pack <$>) $ char '"' *> manyTill anyChar (char '"')

schemeComment :: Parsec Text () SchemeToken
schemeComment = (SchemeComment . pack <$>) $ char ';' *> manyTill anyChar (try $ eof <|> void newline)

schemeCharacter :: Parsec Text () SchemeToken
schemeCharacter =
    try backspaceChar <|>
    try tabChar <|>
    try newlineChar <|>
    try linefeedChar <|>
    try pageChar <|>
    try returnChar <|>
    try spaceChar <|>
    try ruboutChar <|>
    try literalSpaceChar <|>
    simpleChar
    where
        prefix = string "#\\"

        backspaceChar    = prefix *> string "backspace" $> SchemeCharacter BackspaceCharacter
        tabChar          = prefix *> string "tab"       $> SchemeCharacter TabCharacter
        newlineChar      = prefix *> string "newline"   $> SchemeCharacter NewlineCharacter
        linefeedChar     = prefix *> string "linefeed"  $> SchemeCharacter LinefeedCharacter
        pageChar         = prefix *> string "page"      $> SchemeCharacter PageCharacter
        returnChar       = prefix *> string "return"    $> SchemeCharacter ReturnCharacter
        spaceChar        = prefix *> string "space"     $> SchemeCharacter SpaceCharacter
        ruboutChar       = prefix *> string "rubout"    $> SchemeCharacter RuboutCharacter
        literalSpaceChar = prefix *> char ' '           $> SchemeCharacter SpaceCharacter

        simpleChar = (SchemeCharacter . SimpleCharacter <$>) $ prefix *> anyChar
