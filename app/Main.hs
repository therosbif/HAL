module Main where

import Control.Exception (SomeException (SomeException))
import Control.Exception.Base (catch)
import Control.Monad (void)
import Data.Foldable (foldr1)
import File (runFile)
import Repl (runRepl)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitSuccess, exitWith)
import System.IO (hPutStr, stderr)

handleErr :: ExitCode -> IO b
handleErr ExitSuccess = exitSuccess
handleErr (ExitFailure 1) = exitWith (ExitFailure 84)
handleErr e = hPutStr stderr (show e) >> exitWith (ExitFailure 84)

handleArgs :: [String] -> IO ()
handleArgs args =
  catch
    ( if null args || elem "-i" args
        then runFile (filter (/= "-i") args) >>= runRepl
        else void (runFile args)
    )
    handleErr

main :: IO ()
main = getArgs >>= handleArgs
