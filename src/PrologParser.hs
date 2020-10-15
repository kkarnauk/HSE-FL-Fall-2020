{-# LANGUAGE FlexibleContexts #-}

module PrologParser where

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.Char (isLower)
import PrologAst

languageDef =
  emptyDef { Token.identStart = lower
           , Token.identLetter = alphaNum <|> char '_'
           , Token.reservedNames = ["module", "type"]
           , Token.reservedOpNames = [",", ";", "->", ":-", "|"]
           }


parseExpr parser = parse (do r <- parser; eof; return r) ""
parsePrologProgram = parseExpr parseProgram


lexer = Token.makeTokenParser languageDef

identifier :: Parser Identifier
identifier = do
  i <- Token.identifier lexer
  guard $ isLower $ head i
  return (Identifier i)

variable :: Parser Variable
variable = do
  spaces
  h <- upper
  t <- many (alphaNum <|> char '_')
  spaces
  return (Variable (h:t))

whiteSpace = Token.whiteSpace lexer
reservedOp = Token.reservedOp lexer
reserved = Token.reserved lexer
parens = Token.parens lexer
braces = Token.squares lexer
dot = Token.dot lexer


parseSequence el sep = try $ do 
  head <- el
  tail <- many (sep >> el)
  return (head : tail)

parseModule :: Parser Identifier
parseModule = try $ do
  _    <- reserved "module"
  name <- identifier
  _    <- dot
  return name


parseAtom :: Parser Atom
parseAtom = try (do
    head <- identifier
    args <- parseAtomArgs
    return (Atom head args)
  ) <|> parseAnyList

parseAtomArgs :: Parser [Arg]
parseAtomArgs = try $ many parseAtomArg

parseAtomArg :: Parser Arg
parseAtomArg = try (parens parseAtomArg)          <|>
  fmap AAtom (try parseAnyList)                   <|>
  fmap AAtom (try (parens parseAtom))             <|>
  fmap (\x -> AAtom (Atom x [])) (try identifier) <|>
  fmap AVar (try variable) 

consAtom = Atom (Identifier "cons")
nilAtom = Atom (Identifier "nil") []

data List = List [Arg]
data ListHT = ListHT Arg Variable

fromListToAtom :: List -> Atom
fromListToAtom (List list) = foldr (\x y -> consAtom [x, AAtom y]) nilAtom list

fromListHTToAtom :: ListHT -> Atom
fromListHTToAtom (ListHT head tail) = consAtom [head, AVar tail]

parseAnyList :: Parser Atom
parseAnyList = (try parseList) <|> (try parseListHT)

parseListArg :: Parser Arg
parseListArg = fmap AAtom (try parseAtom) <|>
  fmap AAtom (try parseAnyList) <|>
  fmap AVar (try variable)

parseList :: Parser Atom
parseList = fmap (fromListToAtom . List) $ try $ braces (
    try (parseSequence parseListArg (reservedOp ",")) <|>
    try (do 
      spaces
      return []
    )
  )

parseListHT :: Parser Atom
parseListHT = fmap fromListHTToAtom $ try $ braces (do
    head <- parseListArg
    _    <- reservedOp "|"
    tail <- variable
    return (ListHT head tail)
  )


parseType :: Parser Type
parseType = fmap (foldr1 Arrow) $ try $ parseSequence parseTypeArg (reservedOp "->")

parseTypeArg :: Parser Type
parseTypeArg = fmap TVar (try variable) <|>
  fmap TAtom (try parseAtom)            <|>
  try (parens parseType)

parseTypeDef :: Parser TypeDef
parseTypeDef = try $ do
  _     <- reserved "type"
  name  <- identifier
  ttype <- parseType
  _     <- dot
  return (TypeDef name ttype)

parseDisjunction :: Parser RelationBody
parseDisjunction = fmap (foldr1 Disj) $ try $ parseSequence parseConjunction (reservedOp ";")

parseConjunction :: Parser RelationBody
parseConjunction = fmap (foldr1 Conj) $ 
  try $ parseSequence (fmap RAtom (try parseAtom) <|> try (parens parseDisjunction)) (reservedOp ",") 

parseRelation :: Parser Relation
parseRelation = try $ do
    head <- parseAtom
    body <- try $ optionMaybe (reservedOp ":-" >> parseDisjunction)
    _    <- dot
    return (Relation head body)

parseProgram :: Parser PrologProgram
parseProgram = try $ do
  spaces
  pmodule  <- try $ optionMaybe parseModule
  spaces
  typeDefs <- many parseTypeDef
  rels     <- many parseRelation
  return (Program pmodule typeDefs rels)
