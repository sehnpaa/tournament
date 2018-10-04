module Helper where

upsert :: Semigroup a => (a -> a -> Bool) -> a -> [a] -> [a]
upsert _ a [] = [a]
upsert isMatch x (y:ys) =
    if isMatch x y
        then x <> y : ys
        else y : upsert isMatch x ys