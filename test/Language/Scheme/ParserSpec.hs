module Language.Scheme.ParserSpec (parserSpec) where

import           Test.Hspec             (Spec, describe, it)
import           Test.Hspec.Parsec      (shouldFailOn, shouldParse)

import           Data.Text              (Text)
import           Text.Heredoc           (str)
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

    describe "quote parser" $ do
        it "should parse quoted tokens" $
            parseEof schemeQuote "'abc" `shouldParse` SchemeQuote (SchemeIdentifier "abc")

        it "should parse nested quotes" $
            parseEof schemeQuote "'''()" `shouldParse` SchemeQuote (SchemeQuote (SchemeQuote (SchemeList [])))

        it "should not parse malformed quotes" $
            parseEof schemeQuote `shouldFailOn` "abc"

    describe "string parser" $ do
        it "should parse string literals" $
            parseEof schemeString "\"aaa\"" `shouldParse` SchemeString "aaa"

        it "should not parse malformed string literals" $
            parseEof schemeString `shouldFailOn` "\"aaa"

    describe "comment parser" $ do
        it "should parse scheme comments" $
            parseEof schemeComment "; THIS IS A COMMENT" `shouldParse` SchemeComment " THIS IS A COMMENT"

        it "should not parse malformed comments" $
            parseEof schemeComment `shouldFailOn` "THIS IS NOT A COMMENT"

    describe "scheme parser" $ do
        it "should parse simple real scheme programs" $ do
            let program =
                    [str|(define (factorial n)
                        |    (if (= n 0)
                        |        1
                        |        (* n (factorial (- n 1)))))
                        |]
                structure =
                    [ SchemeList
                        [ SchemeIdentifier "define"
                        , SchemeList
                            [ SchemeIdentifier "factorial"
                            , SchemeIdentifier "n"
                            ]
                        , SchemeList
                            [ SchemeIdentifier "if"
                            , SchemeList
                                [ SchemeIdentifier "="
                                , SchemeIdentifier "n"
                                , SchemeNumber "0"
                                ]
                            , SchemeNumber "1"
                            , SchemeList
                                [ SchemeIdentifier "*"
                                , SchemeIdentifier "n"
                                , SchemeList
                                    [ SchemeIdentifier "factorial"
                                    , SchemeList
                                        [ SchemeIdentifier "-"
                                        , SchemeIdentifier "n"
                                        , SchemeNumber "1"
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
            parseEof schemeParser program `shouldParse` structure

        it "should parse scheme programs with comments" $ do
            let program =
                    [str|(define (factorial n) ; define factorial :: Int -> Int
                        |    (if (= n 0)
                        |        1             ; if n = 0, return 0
                        |        (* n (factorial (- n 1))))) ; recursive
                        |]
                structure =
                    [ SchemeList
                        [ SchemeIdentifier "define"
                        , SchemeList
                            [ SchemeIdentifier "factorial"
                            , SchemeIdentifier "n"
                            ]
                        , SchemeComment " define factorial :: Int -> Int"
                        , SchemeList
                            [ SchemeIdentifier "if"
                            , SchemeList
                                [ SchemeIdentifier "="
                                , SchemeIdentifier "n"
                                , SchemeNumber "0"
                                ]
                            , SchemeNumber "1"
                            , SchemeComment " if n = 0, return 0"
                            , SchemeList
                                [ SchemeIdentifier "*"
                                , SchemeIdentifier "n"
                                , SchemeList
                                    [ SchemeIdentifier "factorial"
                                    , SchemeList
                                        [ SchemeIdentifier "-"
                                        , SchemeIdentifier "n"
                                        , SchemeNumber "1"
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    , SchemeComment " recursive"
                    ]
            parseEof schemeParser program `shouldParse` structure

        it "should parse scheme programs with quotes" $ do
            let program =
                    [str|(define (map proc lis)
                        |    (cond ((null? lis)
                        |           '())
                        |          ((pair? lis)
                        |           (cons (proc (car lis))))))
                        |]
                structure =
                    [ SchemeList
                        [ SchemeIdentifier "define"
                        , SchemeList
                            [ SchemeIdentifier "map"
                            , SchemeIdentifier "proc"
                            , SchemeIdentifier "lis"
                            ]
                        , SchemeList
                            [ SchemeIdentifier "cond"
                            , SchemeList
                                [ SchemeList
                                    [ SchemeIdentifier "null?"
                                    , SchemeIdentifier "lis"
                                    ]
                                , SchemeQuote (SchemeList [])
                                ]
                            , SchemeList
                                [ SchemeList
                                    [ SchemeIdentifier "pair?"
                                    , SchemeIdentifier "lis"
                                    ]
                                , SchemeList
                                    [ SchemeIdentifier "cons"
                                    , SchemeList
                                        [ SchemeIdentifier "proc"
                                        , SchemeList
                                            [ SchemeIdentifier "car"
                                            , SchemeIdentifier "lis"
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
            parseEof schemeParser program `shouldParse` structure

        it "should not parse scheme programs with unrecognisable characters" $
            parseEof schemeParser `shouldFailOn` "(define x = \\0)"

        it "should not parse malformed scheme programs" $
            parseEof schemeParser `shouldFailOn` "(define x = 0"

parseEof :: Parsec Text () a -> Text -> Either ParseError a
parseEof parser = parse (parser <* eof) ""
