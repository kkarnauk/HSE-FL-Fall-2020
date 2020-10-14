module Main where

import PrologParser
import System.IO
import System.Environment


runParser :: String -> IO ()
runParser str =
  case parsePrologProgram str of
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
  let filename = (args !! 1)
  
  parseFromFile filename