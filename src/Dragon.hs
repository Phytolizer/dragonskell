module Dragon (run) where

import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Dragon.Lexer (newLexer, tokens)
import Options.Applicative (
  Parser,
  ParserResult (..),
  argument,
  defaultPrefs,
  execParserPure,
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  metavar,
  optional,
  progDesc,
  short,
  str,
  strOption,
  switch,
 )
import System.IO (IOMode (ReadMode), hGetContents, openFile)

data OutputMode
  = OutputExe
  | OutputAsm
  | OutputDumpAst

data RawOptions = RawOptions
  { rawoptInput :: String
  , rawoptOutput :: Maybe String
  , rawoptDumpAst :: Bool
  , rawoptAsm :: Bool
  }

rawOptions :: Parser RawOptions
rawOptions =
  RawOptions
    <$> argument str (metavar "FILE" <> help "The file to compile")
    <*> optional (strOption (metavar "OUTPUT" <> long "output" <> short 'o' <> help "The output file"))
    <*> switch (long "dump-ast" <> help "Dump the AST")
    <*> switch (short 'S' <> help "Output assembly")

data Options = Options
  { optInput :: String
  , optOutput :: String
  , optOutputMode :: OutputMode
  }

options :: RawOptions -> Options
options raw =
  Options
    { optInput = rawoptInput raw
    , optOutput = fromMaybe "a.out" (rawoptOutput raw)
    , optOutputMode = outputMode raw
    }

outputMode :: RawOptions -> OutputMode
outputMode raw
  | rawoptDumpAst raw = OutputDumpAst
  | rawoptAsm raw = OutputAsm
  | otherwise = OutputExe

slurpFile :: String -> IO String
slurpFile path = do
  fh <- openFile path ReadMode
  hGetContents fh

run :: [String] -> IO ()
run args =
  let rawOpts = case execParserPure defaultPrefs opts args of
        Success a -> a
        Failure f -> error (show f)
        CompletionInvoked _ -> error "Completion invoked"
   in let opts = options rawOpts
       in do
            contents <- slurpFile (optInput opts)
            let lexer = newLexer contents (optInput opts)
             in putStrLn (map show (tokens lexer) & unlines)
            return ()
  where
    opts =
      info
        (helper <*> rawOptions)
        ( fullDesc
            <> progDesc "Compile C source"
            <> header "dragon - a C compiler"
        )
