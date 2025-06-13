module Language.Scheme.ParserSpec (parserSpec) where

import           Test.Hspec             (Spec, describe, it)
import           Test.Hspec.Parsec      (shouldFailOn, shouldParse)

import           Data.Text              (Text)
import           Text.Parsec            (ParseError, Parsec, eof, parse)

import           Language.Scheme.Parser

parserSpec :: Spec
parserSpec = do
    describe "identifier parser" $ do
        it "should parse an identifier consisting of alphabets" $
            parseEof schemeIdentifier "intercalate" `shouldParse` SchemeIdentifier "intercalate"

        it "should parse an identifier consisting of alphabets and numbers" $
            parseEof schemeIdentifier "liftM2" `shouldParse` SchemeIdentifier "liftM2"

        it "should parse an identifier consisting of alphabets and symbols" $
            parseEof schemeIdentifier "celsius->fahrenheit" `shouldParse` SchemeIdentifier "celsius->fahrenheit"

        it "should not parse an unrecognisable character" $
            parseEof schemeIdentifier `shouldFailOn` "\\"

    describe "number parser" $ do
        it "should parse a number" $
            parseEof schemeNumber "123456" `shouldParse` SchemeNumber "123456"

        it "should not parse a malformed number" $
            parseEof schemeNumber `shouldFailOn` "1234a"

    describe "list parser" $ do
        it "should parse a list with an element" $
            parseEof schemeList "(define)" `shouldParse` SchemeList [SchemeIdentifier "define"]

        it "should parse a list with two elements" $
            parseEof schemeList "(let x)" `shouldParse` SchemeList [SchemeIdentifier "let", SchemeIdentifier "x"]

        it "should parse a list with three elements" $
            parseEof schemeList "(let x 5)" `shouldParse` SchemeList [SchemeIdentifier "let", SchemeIdentifier "x", SchemeNumber "5"]

        it "should parse a list with no elements" $
            parseEof schemeList "()" `shouldParse` SchemeList []

        it "should parse lists inside an expression" $
            parseEof schemeList "((let x (6)) abc)" `shouldParse`
                SchemeList [SchemeList [SchemeIdentifier "let", SchemeIdentifier "x", SchemeList [SchemeNumber "6"]], SchemeIdentifier "abc"]

        it "should not parse a malformed list" $
            parseEof schemeList `shouldFailOn` "("

parseEof :: Parsec Text () a -> Text -> Either ParseError a
parseEof parser = parse (parser >>= \result -> eof >> return result) ""
