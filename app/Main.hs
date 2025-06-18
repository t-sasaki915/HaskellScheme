module Main (main) where

import qualified Data.Text            as Text
import qualified Data.Text.IO         as TextIO
import           Language.Scheme.Eval (evalScheme)
import           Options.Applicative

newtype AppOption = AppOption
    { sourceFilePath :: Maybe FilePath
    }

argumentParser :: Parser AppOption
argumentParser = AppOption
    <$> optional
        ( strOption
            ( long "src"
           <> short 's'
           <> help "Filepath to Scheme source to interpret."
           <> metavar "FilePath"
            )
        )

appMain :: AppOption -> IO ()
appMain opts =
    case sourceFilePath opts of
        Just srcPath ->
            TextIO.readFile srcPath >>=
                evalScheme >>=
                    TextIO.putStrLn . Text.show

        Nothing ->
            error "Not implemented"

main :: IO ()
main = appMain =<< execParser opts
    where
        opts = info (argumentParser <**> helper)
            ( fullDesc
           <> header "HaskellScheme Interpreter"
            )
