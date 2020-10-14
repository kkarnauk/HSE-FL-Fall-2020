module Test.PrologParser where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import Text.ParserCombinators.Parsec
import Data.Either (isLeft)

import PrologParser
import PrologAst

parseString :: Parser a -> String -> Either ParseError a
parseString p =
  parse (do r <- p; eof; return r) ""

testParserSuccess :: (Eq a, Show a) => Parser a -> String -> a -> Assertion
testParserSuccess p inp exp =
  parseString p inp @?= Right exp

testParserFailure :: (Eq a, Show a) => Parser a -> String -> Assertion
testParserFailure p inp =
  assertLeft $ parseString p inp

assertLeft :: (Show a, Show b) => Either a b -> Assertion
assertLeft x =
  assertBool ("expected: Left\n but got: " ++ show x) (isLeft x)

unit_identifier :: Assertion
unit_identifier = do
  let parser = identifier
  let success = testParserSuccess parser
  let fail  = testParserFailure parser
  success "abc" (Identifier "abc")
  success "aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPrRsStTuUvVwWxXyYzZ_1234567890"
          (Identifier "aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPrRsStTuUvVwWxXyYzZ_1234567890")
  fail "123abc"
  fail "Xyz"

unit_variable :: Assertion
unit_variable = do
  let parser = variable
  let success = testParserSuccess parser
  let fail  = testParserFailure parser
  success "Abc" (Variable "Abc")
  success "H" (Variable "H")
  success "AabBcCdDeEfFgGhHiIjJkKlLmMnNoOpPrRsStTuUvVwWxXyYzZ_1234567890"
          (Variable "AabBcCdDeEfFgGhHiIjJkKlLmMnNoOpPrRsStTuUvVwWxXyYzZ_1234567890")
  fail "123abc"
  fail "xyz"

unit_manyIdent :: Assertion
unit_manyIdent = do
  let parser = many identifier
  let success = testParserSuccess parser
  let fail  = testParserFailure parser
  success "a b c" [Identifier "a", Identifier "b", Identifier "c"]

a = Atom (Identifier "a")
b = Atom (Identifier "b")
c = Atom (Identifier "c")
d = Atom (Identifier "d")
f = Atom (Identifier "f")

a' = a []
b' = b []
c' = c []
d' = d []
f' = f []

vx = Variable "X"
vy = Variable "Y"
vz = Variable "Z"

unit_atom :: Assertion
unit_atom = do
  let parser = parseAtom
  let success = testParserSuccess parser
  let fail  = testParserFailure parser
  success "a" a'
  success "a b c" (a [AAtom b', AAtom c'])
  success "a (b c)" (a [AAtom $ b [AAtom c']])
  success "a ((b c))" (a [AAtom $ b [AAtom c']])
  success "a ((b c)) d" (a [AAtom $ b [AAtom c'], AAtom d'])
  success "a ((b c))  (d)" (a [AAtom $ b [AAtom c'], AAtom d'])
  success "a ((b  c))  \t\t(d\n)" (a [AAtom $ b [AAtom c'], AAtom d'])
  success "hello ((b  c) )  ( d )" (Atom (Identifier "hello") [AAtom $ b [AAtom c'], AAtom d'])
  success "a((b c))(d)" (a [AAtom $ b [AAtom c'], AAtom d'])
  success "a \t\t (\t(X)) (Y) (((Y)))" (a [AVar vx, AVar vy, AVar vy])
  fail "a (a"
  fail "X a"
  fail "(a)"
  fail "a ((a) a)"
  fail "xy (X a b) c"
  fail "aXaX (a (a ((a) a)))"


unit_module :: Assertion
unit_module = do
  let parser = parseModule
  let success = testParserSuccess parser
  let fail  = testParserFailure parser
  success "module a." (Identifier "a")
  success "module     hello_world   ." (Identifier "hello_world")
  success "module \n\n\n\n\n hello   \n\n\n\t\t\t\t .   " (Identifier "hello")
  fail "Module a."
  fail "helo."
  fail "module . hello ."
  fail "module a b.\n\n"
  fail "module X."
  fail "module Hello."
  fail "module a"
  fail "hello module."

alist = Atom (Identifier "list")
alistA = alist [AVar (Variable "A")]
alista = alist [AAtom $ Atom (Identifier "a") []]
atomo = Atom (Identifier "o") []

ta' = TAtom a'
tb' = TAtom b'
tc' = TAtom c'

unit_list :: Assertion
unit_list = do
  let parser = parseList
  let success = testParserSuccess parser
  let fail  = testParserFailure parser
  success "[]" (List [])
  success "[  \t\t\n \n]" (List [])
  success "[a X a]" (List [AAtom $ a [AVar vx, AAtom a']])
  success "[a, b, c]" (List [AAtom a', AAtom b', AAtom c'])
  success "[a, b c, a]" (List [AAtom a', AAtom $ b [AAtom c'], AAtom a'])
  success "[[]\t\t]" (List [AList $ RList $ List []])
  success "[[a, b], X]" (List [AList $ RList $ List [AAtom a', AAtom b'], AVar vx])
  success "[[], [], [X]]" (List [AList $ RList $ List [], AList $ RList $ List [], AList $ RList $ List [AVar vx]])
  success "[[X | \n\nY], a]" (List [AList $ RListHT $ ListHT (AVar vx) vy, AAtom a'])
  fail "[a, b, c"
  fail "[)"
  fail "[a, B; c]"
  fail "[aa (aa), [], [], ]" 
  fail "[, b]"
  fail "[a, , b]"

unit_listHT :: Assertion
unit_listHT = do
  let parser = parseListHT
  let success = testParserSuccess parser
  let fail  = testParserFailure parser
  success "[X \t| Y]" (ListHT (AVar vx) vy)
  success "[a X ((c\n)) | Y]" (ListHT (AAtom $ a [AVar vx, AAtom c']) vy)
  success "[[a\n, b] | X]" (ListHT (AList $ RList $ List [AAtom a', AAtom b']) vx)
  fail "[H | T | C]"
  fail "[X | ]"
  fail "[F | a]"
  fail "[X | Y"
  fail "|Y]"

unit_atom_list :: Assertion
unit_atom_list = do
  let parser = parseAtom
  let success = testParserSuccess parser
  let fail  = testParserFailure parser
  success "a [] []" (a [AList $ RList $ List [], AList $ RList $ List []])
  success "a [a, b ([a, b]), c] (a [b | X]) [[] | X]"
    (a [AList $ RList $ List [AAtom a', AAtom $ b [AList $ RList $ List [AAtom a', AAtom b']], AAtom c'], 
      AAtom $ a [AList $ RListHT $ ListHT (AAtom b') vx], AList $ RListHT $ ListHT (AList $ RList $ List []) vx])
  fail "[X | Y] b"
  fail "[a, b] a"
  fail "[]"
  fail "a [b, c"

unit_type :: Assertion
unit_type = do
  let parser = parseType
  let success = testParserSuccess parser
  let fail  = testParserFailure parser
  success "a" (TAtom a')
  success "Y -> X" (Arrow (TVar vy) (TVar vx))
  success "Z -> X -> Y -> X" (Arrow (TVar vz) (Arrow (TVar vx) (Arrow (TVar vy) (TVar vx))))
  success "list A" (TAtom alistA)
  success "list A -> list ((list)) -> a" 
      (Arrow (TAtom alistA) (Arrow (TAtom $ alist [AAtom $ alist []]) ta'))
  success "X -> (c -> (a -> b)) -> ((b -> a))" 
      (Arrow (TVar vx) (Arrow (Arrow tc' (Arrow ta' tb')) (Arrow tb' ta')))
  success "a -> ((a))" (Arrow ta' ta')
  success "((a -> a)) -> (((Xab))) -> c" (Arrow (Arrow ta' ta') (Arrow (TVar $ Variable "Xab") tc')) 
  fail "X -> Y ->"
  fail "XX -> (x ->) -> o"
  fail "X -> ("
  fail ") X"
  fail "hello :- hello"
  fail "-> hey"

unit_typeDef :: Assertion
unit_typeDef = do
  let parser = parseTypeDef
  let success = testParserSuccess parser
  let fail  = testParserFailure parser
  success "type a b." (TypeDef (Identifier "a") (TAtom b'))
  success "type a b -> X." (TypeDef (Identifier "a") (Arrow (TAtom b') (TVar (Variable "X"))))
  success "type filter (X -> o) -> list a -> list a -> o." 
      (TypeDef (Identifier "filter") (Arrow (Arrow (TVar vx) (TAtom atomo)) (Arrow (TAtom alista) (Arrow (TAtom alista) (TAtom atomo)))))
  success "type filter (X -> o) -> list A -> list A -> o." 
      (TypeDef (Identifier "filter") (Arrow (Arrow (TVar vx) (TAtom atomo)) (Arrow (TAtom alistA) (Arrow (TAtom alistA) (TAtom atomo)))))
  success "type hey ((X -> (a -> b) -> c) -> b) -> o." 
      (TypeDef (Identifier "hey") (Arrow (Arrow (Arrow (TVar vx) (Arrow (Arrow ta' tb') tc')) tb') (TAtom atomo)))
  fail "type type type -> type."
  fail "type x -> y -> z."
  fail "tupe x o."
  fail "typee hey o."
  fail "type Filter x -> y."
  fail "hello hello o."
  fail "type (x -> y) x -> y."
  fail "type x o"

unit_conjunction :: Assertion
unit_conjunction = do
  let parser = parseConjunction
  let success = testParserSuccess parser
  let fail  = testParserFailure parser
  success "a, b, \n\nc" (Conj (RAtom a') (Conj (RAtom b') (RAtom c')))
  success "a, (b c [X])" (Conj (RAtom a') (RAtom $ b [AAtom c', AList $ RList $ List [AVar vx]]))
  fail "a, b,"
  fail ", a, b"
  fail "a, b (a, c)"
  fail "a ,, c"

unit_disjunction :: Assertion
unit_disjunction = do
  let parser = parseDisjunction
  let success = testParserSuccess parser
  let fail  = testParserFailure parser
  success "a ; b ;   \n c\n" (Disj (RAtom a') (Disj (RAtom b') (RAtom c')))
  success "a, (b, c; a [X])" (Conj (RAtom a') (Disj (Conj (RAtom b') (RAtom c')) (RAtom $ a [AList $ RList $ List [AVar vx]])))
  fail "a;"
  fail "a ; b ; (; c)"
  fail "a, b, ; c"
  fail "a ;; b"

unit_relation :: Assertion
unit_relation = do
  let parser = parseRelation
  let success = testParserSuccess parser
  let fail  = testParserFailure parser
  success "a." (Relation a' Nothing)
  success "a b." (Relation (a [AAtom b']) Nothing)
  success "a:-a." (Relation a' (Just (RAtom a')))
  success "a :-a." (Relation a' (Just (RAtom a')))
  success "a:-a b." (Relation a' (Just (RAtom (a [AAtom b']))))
  success "a b:- (a b)  ." (Relation (a [AAtom b']) (Just (RAtom (a [AAtom b']))))
  success "a b:- a;b,c." (Relation (a [AAtom b']) (Just (Disj (RAtom a') (Conj (RAtom b') (RAtom c')))))
  success "a b:- a;(b,c)." (Relation (a [AAtom b']) (Just (Disj (RAtom a') (Conj (RAtom b') (RAtom c')))))
  success "a b:- (a;b),c." (Relation (a [AAtom b']) (Just (Conj (Disj (RAtom a') (RAtom b')) (RAtom c'))))
  success "a b:- a;b;c." (Relation (a [AAtom b']) (Just (Disj (RAtom a') (Disj (RAtom b') (RAtom c')))))
  success "a b:- a,b,c." (Relation (a [AAtom b']) (Just (Conj (RAtom a') (Conj (RAtom b') (RAtom c')))))
  success "a (b (c))  :- (a b) ." (Relation (a [AAtom $ b [AAtom c']]) (Just (RAtom (a [AAtom b']))))
  success "a :- a (((a))) X." (Relation a' (Just $ RAtom $ a [AAtom a', (AVar vx)]))
  fail "(a) :- a."
  fail ":- a."
  fail "f :- f"
  fail "ff :- (a b))."
  fail "ff :- ((a b)."
  fail "(f :- f.)"
  fail "(f :- f)."
  fail "f :- a;."
  fail "f :- a, b, (a, )."
  fail "f :- a, (, b)."
  fail "a :- (a) X."
  fail "f :- ."
  fail "X."

unit_program :: Assertion
unit_program = do
  let parser = parseProgram
  let success = testParserSuccess parser
  let fail  = testParserFailure parser
  success "\n\n\n\t\t\t a\t\t\t\t:- a\t\t\t." 
    (Program Nothing [] [
      Relation a' (Just $ RAtom $ a')
    ])

  success ("\n\n\n\t\t\t module \n\n\n\n example\n\n\n\t  .\n\n\n" ++
          "\ntype \n typeName    X\t\t\t -> \no\n\n." ++
          "type typeName2 Y -> X.\n\n\n\n" ++
          "typeName2 \n\n\n typeName\t\t\t :-\t\t\t a \t(\nX) \t(f f)\n\n\n,\n\n\n f, (f; f)." ++
          "a \n\n\n(b a (\ta)\n\n) :- a a \n(\na a \t(a \na))\t, (a)\n; \nb; (a, b).\n\n\n") 
          (Program (Just $ Identifier "example") 
            [
              (TypeDef (Identifier "typeName") (Arrow (TVar vx) (TAtom atomo))),
              (TypeDef (Identifier "typeName2") (Arrow (TVar vy) (TVar vx)))
            ]
            [
              (Relation (Atom (Identifier "typeName2") [AAtom $ Atom (Identifier "typeName") []]) 
                (Just $ Conj (RAtom $ a [(AVar vx), (AAtom $ f [AAtom f'])]) 
                  (Conj (RAtom f') (Disj (RAtom f') (RAtom f')))  
                )
              ),
              (Relation (a [AAtom $ b [AAtom a', AAtom a']]) 
                (Just $ Disj (Conj (RAtom $ a [AAtom a', (AAtom $ a [AAtom a', AAtom $ a [AAtom a']])]) (RAtom a')) 
                  (Disj (RAtom b') (Conj (RAtom a') (RAtom b')))
                )
              )
            ]
          )
  fail "module a \n f :- f."
  fail "module a. f :- f"
  fail "f :- f. type filter x."