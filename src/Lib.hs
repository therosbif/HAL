module Lib
    ( handleFile
    ) where
import System.IO (readFile)

handleFile :: String -> IO ()
handleFile f = do
    xs <- lines <$> readFile f
    mapM_ putStrLn xs
