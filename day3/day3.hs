import Data.Char (isDigit)

data Parsed = N Int (Int, Int)
            | S Int
            deriving Show

sumPartNumber :: [Parsed] -> [Parsed] -> [Parsed] -> [Parsed] -> Int
sumPartNumber [] _ _ _ = 0
sumPartNumber _ [] [] [] = 0
sumPartNumber ps@(N v (s, e):ps') p1s@(S s1:p1s') [] []
              | s1 < s-1 = sumPartNumber ps p1s' [] []
              | s1 > e+1 = sumPartNumber ps' p1s [] []
              | otherwise = v + sumPartNumber ps' p1s [] []
sumPartNumber ps [] p2s@(S _:_) [] = sumPartNumber ps p2s [] []
sumPartNumber ps [] [] p3s@(S _:_) = sumPartNumber ps p3s [] []
sumPartNumber ps@(N v (s, e):ps') p1s@(S s1:p1s') p2s@(S s2:p2s') []
              | s1 < s-1 = sumPartNumber ps p1s' p2s []
              | s2 < s-1 = sumPartNumber ps p1s p2s' []
              | s1 > e+1 && s2 > e+1 = sumPartNumber ps' p1s p2s []
              | otherwise = v + sumPartNumber ps' p1s p2s []
sumPartNumber ps [] p2s@(S _:_) p3s@(S _:_) = sumPartNumber ps p2s p3s []
sumPartNumber ps p1s@(S _:_) [] p3s@(S _:_) = sumPartNumber ps p1s p3s []
sumPartNumber (S _:ps') p1s p2s p3s = sumPartNumber ps' p1s p2s p3s
sumPartNumber ps (N _ _:p1s') p2s p3s = sumPartNumber ps p1s' p2s p3s
sumPartNumber ps p1s (N _ _:p2s') p3s = sumPartNumber ps p1s p2s' p3s
sumPartNumber ps p1s p2s (N _ _:p3s') = sumPartNumber ps p1s p2s p3s'
sumPartNumber ps@(N v (s, e):ps') p1s@(S s1:p1s') p2s@(S s2:p2s') p3s@(S s3:p3s')
              | s1 < s-1 = sumPartNumber ps p1s' p2s p3s
              | s2 < s-1 = sumPartNumber ps p1s p2s' p3s
              | s3 < s-1 = sumPartNumber ps p1s p2s p3s'
              | s1 > e+1 && s2 > e+1 && s3 > e+1 = sumPartNumber ps' p1s p2s p3s
              | otherwise = v + sumPartNumber ps' p1s p2s p3s

parseLine :: String -> [Parsed]
parseLine = parseLine' 0 []

parseLine' :: Int -> [Char] -> String -> [Parsed]
parseLine' i [] [] = []
parseLine' i ns [] = [N (read $ reverse ns) (i - length ns, i-1)]
parseLine' i [] ('.':s) = parseLine' (i+1) [] s
parseLine' i ns ('.':s) = N (read $ reverse ns) (i - length ns,i-1) : parseLine' (i+1) [] s
parseLine' i ns (c:s) | isDigit c = parseLine' (i+1) (c:ns) s
                      | null ns = S i : parseLine' (i+1) [] s
                      | otherwise = N (read $ reverse ns) (i - length ns, i-1) : S i : parseLine' (i+1) [] s

group :: [[a]] -> [([a], [a], [a])]
group [] = []
group [a] = [(a, a, [])]
group as@(a1:a2:as') = ([], a1, a2) : group' as where
  group' [a1, a2] = [(a1, a2, [])]
  group' (a1:a2:a3:as) = (a1, a2, a3) : group' (a2:a3:as)

main :: IO ()
main = do
  s <- readFile "test_data"
  print $ foldr (\(a, b, c) s -> s + sumPartNumber b a b c) 0 $ group $ parseLine <$> lines s

