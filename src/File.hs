module File
    ( handleFile
    ) where
import System.IO (readFile)
import Builtins (Procedure)

handleFile :: String -> [(String, Procedure)] -> [(String, Procedure)]
handleFile f s = s--do
    --xs <- lines <$> readFile f
    --interpret (unwords xs)
