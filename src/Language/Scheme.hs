module Language.Scheme (SchemeValue (..)) where

import           Data.Text (Text, unpack)

data SchemeValue = SchemeIdentifier Text
                 | SchemeString Text
                 | SchemeInteger Integer
                 | SchemeList [SchemeValue]
                 | SchemeQuote SchemeValue
                 | SchemeCharacter Char
                 | SchemeVoid
                 deriving Eq

instance Show SchemeValue where
    show (SchemeIdentifier identifier) = unpack identifier
    show (SchemeString str)            = show str
    show (SchemeInteger int)           = show int

    show SchemeVoid                    = "#<void>"

    show (SchemeCharacter '\b')        = "#\\backspace"
    show (SchemeCharacter '\t')        = "#\\tab"
    show (SchemeCharacter '\n')        = "#\\newline"
    show (SchemeCharacter '\f')        = "#\\page"
    show (SchemeCharacter '\r')        = "#\\return"
    show (SchemeCharacter ' ')         = "#\\space"
    show (SchemeCharacter c)           = "#\\" ++ [c]

    show (SchemeList atoms) =
        "(" ++ unwords (map show atoms) ++ ")"

    show (SchemeQuote atom) =
        "'" ++ show atom
