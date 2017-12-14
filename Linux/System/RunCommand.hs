module System.RunCommand
 (
  runCrossPlatformCommand
 ) where
import System.Environment
import GHC.IO.Handle
import Control.Applicative
import System.Process

runCrossPlatformCommand :: String -> [String] -> IO(String)
runCrossPlatformCommand comm args = do
 print args
 (_, Just resultHandler, _, _) <-  createProcess (proc comm args) {std_out = CreatePipe}
 hGetContents resultHandler
