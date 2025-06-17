module Language.Scheme.Parser.Internal
    ( SchemeToken (..)
    , SchemeCharacterType (..)
    ) where

import           Data.Text (Text)

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
