{-# LANGUAGE FlexibleContexts #-}

module Main where

import PrologParser
import System.IO
import System.Environment

runParser parser str =
  case parseExpr parser str of
    Left err -> print err
    Right r -> print r

-- parseFromFile :: FilePath -> IO ()
parseFromFile path parser = do
  input <- readFile path
  case parseExpr parser input of
    Left err -> print err
    Right r -> do
      writeFile (path ++ ".out") (show r)


main :: IO ()
main = do
  args <- getArgs
  let filename = (args !! 1)

  if length args < 3 then
    parseFromFile filename parseProgram
  else
    case (args !! 2) of
      "--atom" ->
          parseFromFile filename parseAtom
      "--typeexpr" ->
          parseFromFile filename parseType
      "--type" ->
          parseFromFile filename parseTypeDef
      "--module" ->
          parseFromFile filename parseModule
      "--relation" ->
          parseFromFile filename parseRelation
      "--list" ->
          parseFromFile filename parseAnyList
      "--prog" ->
          parseFromFile filename parseProgram
          
