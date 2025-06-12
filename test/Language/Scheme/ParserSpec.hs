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
            parseEof identifier "intercalate" `shouldParse` Reference "intercalate"

        it "should parse an identifier consisting of alphabets and numbers" $
            parseEof identifier "liftM2" `shouldParse` Reference "liftM2"

        it "should parse an identifier consisting of alphabets and symbols" $
            parseEof identifier "celsius->fahrenheit" `shouldParse` Reference "celsius->fahrenheit"

        it "should not parse unrecognisable characters" $
            parseEof identifier `shouldFailOn` "\\"

    describe "expression parser" $ do
        it "should parse an expression with an element" $
            parseEof expression "(define)" `shouldParse` Evaluation [Reference "define"]

        it "should parse an expression with two elements" $
            parseEof expression "(let x)" `shouldParse` Evaluation [Reference "let", Reference "x"]

        it "should parse an expression with three elements" $
            parseEof expression "(let x =)" `shouldParse` Evaluation [Reference "let", Reference "x", Reference "="]

        it "should parse an expression with no elements" $
            parseEof expression "()" `shouldParse` Evaluation []

        it "should parse expressions inside an expression" $
            parseEof expression "((let x (y)) abc)" `shouldParse` Evaluation [Evaluation [Reference "let", Reference "x", Evaluation [Reference "y"]], Reference "abc"]

        it "should not parse malformed expressions" $
            parseEof expression `shouldFailOn` "("

parseEof :: Parsec Text () a -> Text -> Either ParseError a
parseEof parser = parse (parser >>= \result -> eof >> return result) ""
