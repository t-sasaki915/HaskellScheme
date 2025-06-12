module Language.Scheme.ParserSpec (parserSpec) where

import           Test.Hspec

import           Text.Parsec            (parse)

import           Language.Scheme.Parser

parserSpec :: Spec
parserSpec = do
    describe "identifier parser" $ do
        it "should parse an identifier consisting of alphabets" $
            let expect = Right (Reference "intercalate")
                actual = "intercalate" in
                    parse identifier "" actual `shouldBe` expect

        it "should parse an identifier consisting of alphabets and numbers" $
            let expect = Right (Reference "liftM2")
                actual = "liftM2" in
                    parse identifier "" actual `shouldBe` expect

        it "should parse an identifier consisting of alphabets and symbols" $
            let expect = Right (Reference "celsius->fahrenheit")
                actual = "celsius->fahrenheit" in
                    parse identifier "" actual `shouldBe` expect

        it "should not parse unrecognisable characters" $
            let expect = Right (Reference "aaaa")
                actual = "aaaa\\bbbb" in
                    parse identifier "" actual `shouldBe` expect

    describe "expression parser" $ do
        it "should parse an expression with an element" $
            let expect = Right (Evaluation [Reference "define"])
                actual = "(define)" in
                    parse expression "" actual `shouldBe` expect
        it "should parse an expression with two elements" $
            let expect = Right (Evaluation [Reference "let", Reference "x"])
                actual = "(let x)" in
                    parse expression "" actual `shouldBe` expect

