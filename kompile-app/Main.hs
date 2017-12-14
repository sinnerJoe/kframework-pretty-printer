module Main where

import Lib
import Kfileparser
import System.Environment
import Control.Monad
import System.RunCommand
main :: IO ()
main = do
  args <- getArgs

  if (Prelude.length args == 2) && (hasCreateHtmlArg args) then do
    let path = last args
    result <- runCrossPlatformCommand "kompile" [path]
    when (null result) $ do {parseKFile path ; generateHTMLFile path}
  else if Prelude.length args == 1 then do
        let [path] = args
        result <- runCrossPlatformCommand "kompile" [path]
        when (null result) $ parseKFile path
  else
    putStrLn "[Error] Wrong number of arguments!"
  return ()


hasCreateHtmlArg :: [String] -> Bool
hasCreateHtmlArg (one : rest) = "--html" == one
