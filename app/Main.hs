module Main where

import System.Environment (getArgs)
import File (handleFile)
import Data.Foldable (foldr1)
import System.Exit (exitWith, exitSuccess, ExitCode (ExitFailure))
import System.IO (stderr, hPutStr)
import Control.Exception.Base (catch)
import Control.Exception (SomeException(SomeException))
import Repl (runRepl)

handleArgs :: [String] -> IO ()
handleArgs [] = pure () -- REPL
handleArgs (x:xs) =
  handleArgs xs

main :: IO ()
main = do
  args <- getArgs
  catch
    (if null args || elem "-i" args
      then runRepl
      else handleArgs args)
    (\e ->
      let err = show (e :: SomeException)
      in if err /= "exitSuccess"
          then hPutStr stderr err >> exitWith (ExitFailure 84)
          else exitSuccess)
