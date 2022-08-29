module Dragon.Lexer (newLexer, tokens) where

import Data.Char (isDigit, isLetter)
import qualified Data.Char
import Dragon.Token (
  SourcePos (SourcePos, sourceCol, sourceFilename, sourceLine),
  Token (Token),
  TokenType (TTCloseBrace, TTCloseParen, TTIdentifier, TTKwInt, TTKwReturn, TTOpenBrace, TTOpenParen, TTSemicolon),
 )

data Lexer = Lexer
  { lexerSource :: String
  , lexerTokenStart :: SourcePos
  , lexerTokenStartOfs :: Int
  , lexerPos :: Int
  , lexerFilename :: String
  , lexerLine :: Int
  , lexerCol :: Int
  }

newLexer :: String -> String -> Lexer
newLexer source filename =
  Lexer
    { lexerSource = source
    , lexerTokenStart =
        SourcePos
          { sourceFilename = filename
          , sourceLine = 1
          , sourceCol = 1
          }
    , lexerTokenStartOfs = 0
    , lexerPos = 0
    , lexerFilename = filename
    , lexerLine = 1
    , lexerCol = 1
    }

nextToken :: Lexer -> Maybe (Lexer, Token)
nextToken lexer =
  let lexer = skipWhitespace lexer
   in case peek lexer of
        Nothing -> Nothing
        Just c -> case c of
          '{' -> Just (lexer, addToken lexer TTOpenBrace)
          '}' -> Just (lexer, addToken lexer TTCloseBrace)
          '(' -> Just (lexer, addToken lexer TTOpenParen)
          ')' -> Just (lexer, addToken lexer TTCloseParen)
          ';' -> Just (lexer, addToken lexer TTSemicolon)
          c ->
            if isLetter c
              then Just (lexer, addToken lexer (readIdentOrKw lexer))
              else Nothing

tokens :: Lexer -> [Token]
tokens lexer =
  case nextToken lexer of
    Nothing -> []
    Just (lexer, token) -> token : tokens lexer

readIdentOrKw :: Lexer -> TokenType
readIdentOrKw lexer =
  let ident = readIdent lexer
   in kwKind ident

kwKind :: String -> TokenType
kwKind "int" = TTKwInt
kwKind "return" = TTKwReturn
kwKind s = TTIdentifier s

readIdent :: Lexer -> String
readIdent lexer = loop lexer ""
  where
    loop :: Lexer -> String -> String
    loop lexer acc =
      case peek lexer of
        Nothing -> acc
        Just c ->
          if isLetter c || isDigit c || c == '_'
            then
              let (_, lexer) = advance lexer
               in loop lexer (acc ++ [c])
            else acc

addToken :: Lexer -> TokenType -> Token
addToken lexer ty = Token ty (text lexer) (lexerTokenStart lexer)

sub :: String -> Int -> Int -> String
sub s start end = take (end - start) (drop start s)

text :: Lexer -> String
text lexer = sub (lexerSource lexer) (lexerTokenStartOfs lexer) (lexerPos lexer)

skipWhitespace :: Lexer -> Lexer
skipWhitespace lexer = loop lexer
  where
    loop :: Lexer -> Lexer
    loop lexer =
      let c = peek lexer
       in if isSpace c
            then
              let (_, lexer) = advance lexer
               in loop lexer
            else lexer

advance :: Lexer -> (Maybe Char, Lexer)
advance lexer =
  case peek lexer of
    Just c ->
      if c == '\n'
        then
          ( Just c
          , lexer
              { lexerPos = lexerPos lexer + 1
              , lexerLine = lexerLine lexer + 1
              , lexerCol = 1
              }
          )
        else
          ( Just c
          , lexer
              { lexerPos = lexerPos lexer + 1
              , lexerCol = lexerCol lexer + 1
              }
          )
    Nothing -> (Nothing, lexer)

isSpace :: Maybe Char -> Bool
isSpace (Just c) = Data.Char.isSpace c
isSpace Nothing = False

peek :: Lexer -> Maybe Char
peek lexer =
  if lexerPos lexer >= length (lexerSource lexer)
    then Nothing
    else Just $ lexerSource lexer !! lexerPos lexer
