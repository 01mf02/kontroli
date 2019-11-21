{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

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
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import Data.List (elemIndex, mapAccumL)
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

comment :: AP.Parser ByteString
comment = BS.append <$> AP.string "(;" <*> takeTill2 slashdotAscii rparAscii

type Ident = ByteString

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

data Term
  = Symb Ident
  | BVar Int
  | Appl Term Term
  | Abst Arg Term
  | Prod Arg Term
  | Type
  deriving Show

absts, prods :: [Arg] -> Term -> Term
absts args tm = foldr Abst tm args
prods args tm = foldr Prod tm args

sterm :: AP.Parser Term
sterm =
  parens term <|>
  Type <$ lexeme (string "Type") <|>
  Symb <$> ident <?> "symbol"

term :: AP.Parser Term
term =
  Abst <$> (lambda *> arg <* lamArr) <*> term <|>
  Prod <$> (forall *> arg <* allArr) <*> term <|>
  foldl Appl <$> sterm <*> many sterm
  where
    lambda = lexeme backslash
    forall = lexeme exclamation
    lamArr = lexeme (string "=>")
    allArr = lexeme (string "->")

data DCommand t
  = Definition  (Maybe t) (Maybe t)
  | Theorem     t t
  | Declaration t
  deriving (Foldable, Functor, Traversable, Show)

data Command
  = DCmd Ident [Arg] (DCommand Term)
  | Rule Context Term Term
  deriving Show

type Context = [(Ident, Maybe Term)]

cmdName c = case c of
  DCmd i _ _ -> Just i
  Rule _ _ _ -> Nothing

context :: AP.Parser Context
context =
  lexeme lsqu *> decl `AP.sepBy1'` lexeme comma <* lexeme rsqu
  where decl = (,) <$> ident <*> optional ofTerm

command :: AP.Parser Command
command =
  DCmd <$> (def *> ident) <*> many param <*> (Definition <$> optional ofTerm <*> optional isTerm) <|>
  DCmd <$> (thm *> ident) <*> many param <*> (Theorem <$> ofTerm <*> isTerm) <|>
  DCmd <$> ident <*> many param <*> (Declaration <$> ofTerm) <|>
  Rule <$> AP.option [] context <*> term <* rew <*> term
  where
    def = lexeme (string "def")
    thm = lexeme (string "thm")
    rew = lexeme (string "-->")
    param = parens arg

parseCommands :: BL.ByteString -> [Command]
parseCommands contents =
  if BL.null contents then []
  else case parse (space *> command <* lexeme dot) contents of
    Fail _ _ y       ->
      error ("Failure parsing command: " ++ show (BL.take 80 contents) ++ " ...\n" ++ y)
    Done contents' x -> x : parseCommands contents'

type SymbolTable = HM.HashMap ByteString ByteString
type Bound = [Ident]

scopeArg :: SymbolTable -> Bound -> Arg -> Either Ident Arg
scopeArg symTable bound (maybeId, maybeTm) =
  case maybeTm of
    Nothing -> pure (maybeId, Nothing)
    Just tm -> do
      tm <- scopeTerm symTable bound tm
      pure (maybeId, Just tm)

scopeTerm :: SymbolTable -> Bound -> Term -> Either Ident Term
scopeTerm symTable bound tm =
  case tm of
    Symb id ->
      case elemIndex id bound of
        Just i -> pure $ BVar i
        Nothing ->
          case HM.lookup id symTable of
            Just sym -> pure $ Symb sym
            Nothing -> Left id
    Appl t1 t2 ->
      Appl <$> scopeTerm symTable bound t1 <*> scopeTerm symTable bound t2
    Abst arg tm ->
      Abst <$> scopeArg symTable bound arg <*> scopeTerm symTable (addToBound arg bound) tm
    Prod arg tm ->
      Prod <$> scopeArg symTable bound arg <*> scopeTerm symTable (addToBound arg bound) tm
    Type -> pure Type
  where
    addToBound (maybeId, _) bound =
      case maybeId of
        Just id -> id : bound
        Nothing -> bound

paramDCommand :: [Arg] -> DCommand Term -> DCommand Term
paramDCommand pm dcmd =
  case dcmd of
    Definition ty tm ->
      Definition (fmap (prods pm) ty) (fmap (absts pm) tm)
    Theorem ty tm ->
      Theorem (prods pm ty) (absts pm tm)
    Declaration ty ->
      Declaration (prods pm ty)

scopeDCommand :: SymbolTable -> DCommand Term -> Either Ident (DCommand Term)
scopeDCommand symTable = sequenceA . fmap (scopeTerm symTable [])

data ScopeError
  = Redefinition Ident
  | Undefined Ident
  deriving Show

scopeCommand :: SymbolTable -> Command -> (SymbolTable, Either ScopeError Command)
scopeCommand symTable cmd =
  case cmd of
    DCmd id pm dcmd ->
      if HM.member id symTable
      then (symTable, Left $ Redefinition id)
      else
        let dcmd' = scopeDCommand symTable (paramDCommand pm dcmd)
        in (HM.insert id id symTable, bimap Undefined (DCmd id []) dcmd')
    Rule ctx lhs rhs ->
      (symTable, pure $ Rule ctx lhs rhs) -- TODO

scopeCommands :: [Command] -> (SymbolTable, [Either ScopeError Command])
scopeCommands = mapAccumL scopeCommand HM.empty

showScopeResult :: Either ScopeError Command -> String
showScopeResult = either ((++) "Error: " . show) (show . cmdName)

main :: IO ()
main = do
  [fileName] <- getArgs
  contents   <- BL.readFile fileName
  let cmds = parseCommands contents
  mapM_ (putStrLn . showScopeResult) $ snd $ scopeCommands cmds
