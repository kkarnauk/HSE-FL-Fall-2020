{-# LANGUAGE FlexibleContexts #-}

module Main where

import PrologParser
import System.IO
import System.Environment

runParser parser str =
  case parseExpr parser str of
    Left err -> print err
    Right r -> print r

parseFromFile :: FilePath -> IO ()
parseFromFile path = do
  input <- readFile path
  case parsePrologProgram input of
    Left err -> print err
    Right r -> do
      writeFile (path ++ ".out") (show r)


main :: IO ()
main = do
  args <- getArgs
  let command = (args !! 1)
  case command of
    "--atom" -> (do 
        input <- getContents
        runParser parseAtom input
      )
    "--typeexpr" -> (do
        input <- getContents
        runParser parseType input
      )
    "--module" -> (do
        input <- getContents
        runParser parseModule input
      )
    "--relation" -> (do
        input <- getContents
        runParser parseRelation input
      )
    "--list" -> (do
        input <- getContents
        runParser parseList input
      )
    "--prog" -> (do
        input <- getContents
        runParser parseProgram input
      )
    _ -> parseFromFile command
