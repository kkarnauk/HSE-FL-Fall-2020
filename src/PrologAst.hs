module PrologAst where

import Data.List

data Token = Ident String
           | Var String
           | Comma
           | Semi
           | Lbr
           | Rbr
           | Dot
           | Cork
           deriving (Eq, Show)

newtype Variable = Variable String
                 deriving (Eq)

newtype Identifier = Identifier String
                   deriving (Eq)

data Arg = AAtom Atom
         | AList AnyList
         | AVar Variable
         deriving (Eq)

data AnyList = RList List
             | RListHT ListHT
             deriving (Eq)

data List = List [Arg]
          deriving (Eq)

data ListHT = ListHT Arg Variable
            deriving (Eq)

data PrologProgram = Program {
        pModule :: Maybe Identifier
      , types   :: [TypeDef]
      , rels    :: [Relation]
      }
      deriving (Eq)

data TypeDef = TypeDef Identifier Type
             deriving (Eq)

data Type = TVar Variable
          | TAtom Atom
          | Arrow Type Type
          deriving (Eq)


data Atom = Atom { atomHead :: Identifier, atomArgs :: [Arg] }
          deriving (Eq)

data Relation = Relation { relHead :: Atom, relBody :: Maybe RelationBody }
              deriving (Eq)

data RelationBody = RAtom Atom
                  | Conj RelationBody RelationBody
                  | Disj RelationBody RelationBody
                  deriving (Eq)

consAtom = Atom (Identifier "cons")

fromListToAtom :: List -> Arg
fromListToAtom (List [x]) = x
fromListToAtom (List (x : xs)) = AAtom $ consAtom [x, fromListToAtom (List xs)] 

ident = "|  "

applyIdent :: String -> String
applyIdent str = intercalate "\n" $ map (ident ++) (lines str)


instance Show Variable where
  show (Variable x) = "|" ++ show x

instance Show Identifier where
  show (Identifier x) = "|" ++ show x

instance Show Arg where
  show (AAtom atom) = show atom
  show (AList list) = show list
  show (AVar var)   = show var

instance Show AnyList where
  show (RList list)   = show list
  show (RListHT list) = show list

instance Show Atom where
  show (Atom head args) = "|Atom\n" ++ (ident ++ show head ++ "\n") ++ (intercalate "\n" $ map (applyIdent . show) args)

instance Show List where
  show (List []) = "|EmptyList"
  show list = show (fromListToAtom list)

instance Show ListHT where
  show (ListHT head tail) = show (consAtom [head, AVar tail])

instance Show Type where
  show (TVar var) = "|Type\n" ++ applyIdent (show var)
  show (TAtom atom) = "|Type\n" ++ applyIdent (show atom)
  show (Arrow first second) = "|TypeArrow\n" ++ (applyIdent $ show first) ++ "\n" ++ (applyIdent $ show second)

instance Show TypeDef where
  show (TypeDef name ttype) = "|TypeDef\n" ++ (applyIdent $ "|Name\n" ++ applyIdent (show name)) ++ "\n" ++ applyIdent (show ttype) 

instance Show Relation where
  show (Relation head Nothing)     = "|Relation\n" ++ (applyIdent $ "|Head\n" ++ applyIdent (show head))
  show (Relation head (Just body)) = "|Relation\n" ++ (applyIdent $ "|Head\n" ++ applyIdent (show head)) ++ "\n" ++
    (applyIdent $ "|Body\n" ++ applyIdent (show body))

instance Show RelationBody where
  show (RAtom atom) = show atom
  show (Conj first second) = "|Conj\n" ++ (applyIdent $ show first) ++ "\n" ++ (applyIdent $ show second)
  show (Disj first second) = "|Disj\n" ++ (applyIdent $ show first) ++ "\n" ++ (applyIdent $ show second)


instance Show PrologProgram where
  show (Program pModule types rels) = "|Program\n" ++
    (ident ++ "|Module\n")   ++ ident ++ ident ++ (case pModule of { Nothing -> "Nothing"; Just name -> show name }) ++ "\n" ++
    (ident ++ "|TypeDefs\n")  ++ (concatMap ((++ "\n") . applyIdent . applyIdent . show) types) ++ 
    (ident ++ "|Relations\n") ++ (concatMap ((++ "\n") . applyIdent . applyIdent . show) rels)

-- instance Show List where
