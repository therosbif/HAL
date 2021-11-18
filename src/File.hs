module File
    ( handleFile
    ) where
import System.IO (readFile)
import Expr (Procedure)

handleFile :: String -> String
handleFile f = f--do
    --xs <- lines <$> readFile f
    --interpret (unwords xs)
