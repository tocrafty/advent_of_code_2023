import Data.IntSet
import Tools

main :: IO ()
main = do
  s <- readFile "test_data"
  print $ sum $ score <$> lines s

score :: String -> Int
score s =
  let (_:s':_) = Tools.split s (==':')
      (xs':ys':_) = dropWhile (==' ') <$> Tools.split s' (=='|')
      xs = fromList $ read <$> Prelude.filter (/="") (Tools.split xs' (==' '))
      ys = read <$> Prelude.filter (/="") (Tools.split ys' (==' '))
  in winningScore $ Prelude.filter (`member` xs) ys

winningScore :: [Int] -> Int
winningScore [] = 0
winningScore (_:xs) = 2 ^ length xs
