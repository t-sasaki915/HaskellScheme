module Language.Scheme.Eval.Internal (evalTokens) where

import           Data.Maybe                      (mapMaybe, fromJust)
import qualified Data.Text                       as Text
import           Language.Scheme                 (SchemeValue (..))
import           Language.Scheme.Parser.Internal (isComment)
import qualified Language.Scheme.Parser.Internal as Token

evalTokens :: [Token.SchemeToken] -> [SchemeValue]
evalTokens = mapMaybe evalToken . filter (not . isComment)

evalToken :: Token.SchemeToken -> Maybe SchemeValue
evalToken = \case
    Token.SchemeIdentifier str ->
        Just (SchemeIdentifier str)

    Token.SchemeNumber str ->
        Just (SchemeInteger $ read (Text.unpack str)) -- TODO

    Token.SchemeList atoms ->
        Just (SchemeList $ mapMaybe evalToken atoms)

    Token.SchemeQuote atom ->
        Just (SchemeQuote $ fromJust (evalToken atom))

    Token.SchemeString str ->
        Just (SchemeString str)

    Token.SchemeCharacter (Token.SimpleCharacter c) ->
        Just (SchemeCharacter c)

    Token.SchemeCharacter Token.BackspaceCharacter ->
        Just (SchemeCharacter '\b')

    Token.SchemeCharacter Token.TabCharacter ->
        Just (SchemeCharacter '\t')

    Token.SchemeCharacter Token.NewlineCharacter ->
        Just (SchemeCharacter '\n')

    Token.SchemeCharacter Token.LinefeedCharacter ->
        Just (SchemeCharacter '\n')

    Token.SchemeCharacter Token.PageCharacter ->
        Just (SchemeCharacter '\f')

    Token.SchemeCharacter Token.ReturnCharacter ->
        Just (SchemeCharacter '\r')

    Token.SchemeCharacter Token.SpaceCharacter ->
        Just (SchemeCharacter ' ')

    Token.SchemeComment _ ->
        Nothing
