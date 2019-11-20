{-# LANGUAGE OverloadedStrings #-}

{-
Sources:
http://jakewheat.github.io/intro_to_parsing/
http://newartisans.com/2012/08/parsing-with-haskell-and-attoparsec/
https://chrisdone.com/posts/fast-haskell-c-parsing-xml/
https://www.ianthehenry.com/posts/decoding-utf-8/
https://stackoverflow.com/questions/19208231/attoparsec-or-parsec-in-haskell
https://www.reddit.com/r/haskell/comments/6dwutk/chomsky_hierarchy_parsec/
-}

module Main where

import Control.Applicative ((<|>), many, optional)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (mapMaybe)
import Data.Word (Word8)
import Data.Attoparsec.ByteString.Lazy as AP
import Data.Attoparsec.ByteString.Lazy ((<?>))
import System.Environment (getArgs)

takeTill2 p q =
  BS.snoc <$> AP.scan False f <*> AP.satisfy q
  where
    f pLast word =
      if pLast && q word then Nothing
      else Just $ p word

uppercaseAscii x = 65 <= x && x <= 90
lowercaseAscii x = 97 <= x && x <= 122
numericAscii x = 48 <= x && x <= 57
-- TAB, LF (line feed), or space
spaceAscii x = x == 9 || x == 10 || x == 32
starAscii x = x == 42
commaAscii x = x == 44
dotAscii x = x == 46
exclamationAscii x = x == 33
underscoreAscii x = x == 95
lparAscii x = x == 40
rparAscii x = x == 41
lsquAscii x = x == 91
rsquAscii x = x == 93
backslashAscii x = x == 92
colonAscii x = x == 58
slashdotAscii x = x == 59
pipeAscii x = x == 124
rbrackAscii x = x == 125

alpha x = uppercaseAscii x || lowercaseAscii x
alnum x = alpha x || numericAscii x || underscoreAscii x

underscore = AP.skip underscoreAscii
whitespace = AP.skipWhile spaceAscii
lpar = AP.skip lparAscii
rpar = AP.skip rparAscii
colon = AP.skip colonAscii
backslash = AP.skip backslashAscii
exclamation = AP.skip exclamationAscii
comma = AP.skip commaAscii
dot = AP.skip dotAscii
lsqu = AP.skip lsquAscii
rsqu = AP.skip rsquAscii

space = whitespace >> many (comment >> whitespace)

parens p = lexeme lpar *> p <* lexeme rpar <?> "parens"


lexeme :: AP.Parser a -> AP.Parser a
lexeme p = do
  x <- p
  space
  return x

comment :: AP.Parser BS.ByteString
comment = BS.append <$> AP.string "(;" <*> takeTill2 slashdotAscii rparAscii

type Ident = BS.ByteString

bracketIdent :: AP.Parser Ident
bracketIdent = BS.append <$> AP.string "{|" <*> takeTill2 pipeAscii rbrackAscii

normalIdent :: AP.Parser Ident
normalIdent = BS.cons <$> AP.satisfy alpha <*> AP.takeWhile alnum

ident :: AP.Parser Ident
ident = lexeme (bracketIdent <|> normalIdent)

maybeIdent :: AP.Parser (Maybe Ident)
maybeIdent =
  Just <$> ident <|>
  Nothing <$ lexeme underscore



ofTerm, isTerm :: AP.Parser Term
ofTerm  = lexeme colon *> term
isTerm  = lexeme defas *> term
  where defas = AP.string ":="


type Arg = (Maybe Ident, Maybe Term)

arg :: AP.Parser Arg
arg = (,) <$> maybeIdent <*> optional ofTerm

data Binder = Abst | Prod
  deriving Show

data Term
  = Symb Ident
  | Appl Term Term
  | Abs Arg Term
  | Prd Arg Term
  | Type
  deriving Show

sterm :: AP.Parser Term
sterm =
  parens term <|>
  Type <$ lexeme (string "Type") <|>
  Symb <$> ident <?> "symbol"

term :: AP.Parser Term
term =
  Abs  <$> (lambda *> arg <* lamArr) <*> term <|>
  Prd  <$> (forall *> arg <* allArr) <*> term <|>
  foldl Appl <$> sterm <*> many sterm
  where
    lambda = lexeme backslash
    forall = lexeme exclamation
    lamArr = lexeme (string "=>")
    allArr = lexeme (string "->")


type Param = (Ident, Term)

param = parens ((,) <$> ident <*> ofTerm)

data Command
  = Definition  Ident [Param] (Maybe Term) (Maybe Term)
  | Theorem     Ident [Param] Term Term
  | Declaration Ident [Param] Term
  | Rule Context Term Term
  deriving Show

type Context = [(Ident, Maybe Term)]

cmdName c = case c of
  Definition i _ _ _ -> Just i
  Theorem    i _ _ _ -> Just i
  Declaration i _ _ -> Just i
  Rule _ _ _ -> Nothing

context :: AP.Parser Context
context =
  lexeme lsqu *> decl `AP.sepBy1'` lexeme comma <* lexeme rsqu
  where decl = (,) <$> ident <*> optional ofTerm

command =
  (Definition <$> (def *> ident) <*> many param <*> optional ofTerm <*> optional isTerm) <|>
  (Theorem <$> (thm *> ident) <*> many param <*> ofTerm <*> isTerm) <|>
  (Declaration <$> ident <*> many param <*> ofTerm) <|>
  (Rule <$> AP.option [] context <*> term <* rew <*> term)
  where
    def = lexeme (string "def")
    thm = lexeme (string "thm")
    rew = lexeme (string "-->")

commands contents =
  if BL.null contents then []
  else case parse (space *> command <* lexeme dot) contents of
    Fail _ _ y       ->
      error ("Failure parsing command: " ++ show (BL.take 80 contents) ++ " ...\n" ++ y)
    Done contents' x -> x : commands contents'

main :: IO ()
main = do
  [fileName] <- getArgs
  contents   <- BL.readFile fileName
  mapM_ BSC.putStrLn $ mapMaybe cmdName $ commands contents
