module Main (main) where

import qualified Data.Text              as Text
import qualified Data.Text.IO           as TextIO
import           Language.Scheme.Parser (parseScheme)

main :: IO ()
main = TextIO.putStrLn $ Text.show $ parseScheme "(asdf) aaa"
