import           Test.Hspec                 (hspec)

import           Language.Scheme.ParserSpec (parserSpec)

main :: IO ()
main = hspec $ do
    parserSpec
