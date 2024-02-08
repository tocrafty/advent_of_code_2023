module Tools where

split :: [a] -> (a -> Bool) -> [[a]]
split [] _ = [[]]
split (a:as) p = if p a then []:split as p else
  let (s:ss) = split as p in (a:s):ss

