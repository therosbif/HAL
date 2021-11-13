module Utils where

pairs :: (a -> a -> Bool) -> [a] -> Bool
pairs f (x:y:t)  = f x y && pairs f (y:t)
pairs f t        = True
