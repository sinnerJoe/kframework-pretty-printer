module Main where

import Lib
import Kfileparser
import System.Environment
import GHC.IO.Handle
import Control.Applicative
import System.Process
import Control.Monad
import System.RunCommand
main :: IO ()
main = do
  args <- getArgs

  if (Prelude.length args == 2) && (hasCreateHtmlArg args) then do
    let path = last args
    result <- runKompile [path]
    when (null result) $ do {parseKFile path ; generateHTMLFile path}
  else do
    let [path] = args
    result <- runCrossPlatformCommand "kompile" [path]
    when (null result) $ parseKFile path
  return ()


runKompile :: [String] -> IO(String)
runKompile args = do
  print args
  (_, Just resultHandler, _, _) <-  createProcess (shell $  unwords $ "kompile" : args) {std_out = CreatePipe}
  hGetContents resultHandler

hasCreateHtmlArg :: [String] -> Bool
hasCreateHtmlArg (one : rest) = "--html" == one
