module Main where

import System.Environment (getArgs)
import File (handleFile)
import Data.Foldable (foldr1)
import System.Exit (exitWith, exitSuccess, ExitCode (ExitFailure))
import System.IO (stderr, hPutStr)
import Control.Exception.Base (catch)
import Control.Exception (SomeException(SomeException))
import Builtins (Procedure)

handleArgs :: [String] -> [(String, Procedure)] -> IO ()
handleArgs [] _ = pure () -- REPL
handleArgs (x:xs) s =
  let
    symbols = handleFile x s
  in handleArgs xs symbols

main :: IO ()
main = do
  args <- getArgs
  catch (if not $ null args then handleArgs args [] else putStrLn "REPL")
    (\e ->  let err = show (e :: SomeException)
            in if err /= "exitSuccess"
                then hPutStr stderr err >> exitWith (ExitFailure 84)
                else exitSuccess)
