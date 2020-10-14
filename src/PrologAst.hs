module PrologAst where

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
                 deriving (Eq, Show)

newtype Identifier = Identifier String
                   deriving (Eq, Show)

data List = List
          deriving (Eq, Show)

data PrologProgram = Program {
        pModule :: Maybe Identifier
      , types   :: [TypeDef]
      , rels    :: [Relation]
      }
      deriving (Eq, Show)

data TypeDef = TypeDef Identifier Type
             deriving (Eq, Show)

data Type = TVar Variable
          | TAtom Atom
          | Arrow Type Type
          deriving (Eq, Show)

data AtomArg = AAtom Atom
             | AList List
             | AVar Variable
             deriving (Eq, Show)


data Atom = Atom { atomHead :: Identifier, atomArgs :: [AtomArg] }
          deriving (Eq, Show)

data Relation = Relation { relHead :: Atom, relBody :: Maybe RelationBody }
              deriving (Eq, Show)

data RelationBody = RAtom Atom
                  | Conj RelationBody RelationBody
                  | Disj RelationBody RelationBody
                  deriving (Eq, Show)