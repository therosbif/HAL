module Main where

import System.Environment (getArgs)
import Lib (handleFile)
import Data.Foldable (foldr1)
import System.Exit (exitWith, exitSuccess, ExitCode (ExitFailure))
import System.IO (stderr, hPutStr)
import Control.Exception.Base (catch)
import Control.Exception (SomeException(SomeException))

handleArgs :: [String] -> IO ()
handleArgs [] = pure ()
handleArgs (x:xs) =
  catch (handleFile x)
    (\e ->  let err = show (e :: SomeException)
            in if err /= "exitSuccess"
                then hPutStr stderr err >> exitWith (ExitFailure 84)
                else exitSuccess)

main :: IO ()
main = getArgs >>= handleArgs
