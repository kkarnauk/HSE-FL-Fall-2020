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

data Arg = AAtom Atom
         | AList AnyList
         | AVar Variable
         deriving (Eq, Show)

data AnyList = RList List
             | RListHT ListHT
             deriving (Eq, Show)

data List = List [Arg]
          deriving (Eq, Show)

data ListHT = ListHT Arg Variable
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


data Atom = Atom { atomHead :: Identifier, atomArgs :: [Arg] }
          deriving (Eq, Show)

data Relation = Relation { relHead :: Atom, relBody :: Maybe RelationBody }
              deriving (Eq, Show)

data RelationBody = RAtom Atom
                  | Conj RelationBody RelationBody
                  | Disj RelationBody RelationBody
                  deriving (Eq, Show)