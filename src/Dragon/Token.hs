module Dragon.Token where

import Text.Printf (printf)

data TokenType
  = TTOpenBrace
  | TTCloseBrace
  | TTOpenParen
  | TTCloseParen
  | TTSemicolon
  | TTKwInt
  | TTKwReturn
  | TTIdentifier String
  | TTNumber Int
  deriving (Show)

data SourcePos = SourcePos
  { sourceFilename :: String
  , sourceLine :: Int
  , sourceCol :: Int
  }

data Token = Token
  { tokenType :: TokenType
  , tokenText :: String
  , tokenPos :: SourcePos
  }

instance Show Token where
  show (Token ty text pos) =
    printf
      "%s:%d:%d: %s: '%s'"
      (sourceFilename pos)
      (sourceLine pos)
      (sourceCol pos)
      (show ty)
      text
