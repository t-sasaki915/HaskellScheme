module Language.Scheme.Eval (evalScheme) where

import           Control.Monad.Trans.Except      (ExceptT, runExceptT, throwE)
import           Data.Functor                    ((<&>))
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Language.Scheme                 (SchemeValue (..))
import           Language.Scheme.Eval.Internal   (evalTokens)
import           Language.Scheme.Parser          (parseScheme)
import qualified Language.Scheme.Parser.Internal as Token

evalScheme :: Text -> IO (Either Text SchemeValue)
evalScheme src = runExceptT $ do
    values <- parseSrc src <&> evalTokens

    pure (SchemeString $ Text.show values)

parseSrc :: Monad m => Text -> ExceptT Text m [Token.SchemeToken]
parseSrc src =
    case parseScheme src of
        Right tokens -> pure tokens
        Left err     -> throwE (Text.show err)
