import Data.List (stripPrefix)

data Turn = Turn { red   :: Int
                 , blue  :: Int
                 , green :: Int
                 } deriving Show

data Game = Game Int [Turn] deriving Show

r12g13b14 :: Game -> Bool
r12g13b14 (Game _ ts) = all (\t -> red t   <= 12
                                && green t <= 23
                                && blue t  <= 14
                            ) ts

instance Read Turn where
  readsPrec _ s = [(Turn r b g, s')] where
    (s'', s') = span (/= ';') s
    (r, b, g) = foldr ((\s (r, b, g) -> case split s (==' ') of
                                          (n:"red":_)   -> (r+read n, b, g)
                                          (n:"blue":_)  -> (r, b+read n, g)
                                          (n:"green":_) -> (r, b, g+read n)
                                          _             -> error "invalid input"
                       ) . dropWhile (==' ')
                    ) (0, 0, 0)
                    $ split s'' (==',')


instance Read Game where
  readsPrec _ s = [(Game id ts, concat s'')] where
    (g:s':s'') = split s (==':')
    Just id = read <$> stripPrefix "Game " g
    ts = read <$> split s' (==';')

id (Game n _) = n

sumID :: [Game] -> Int
sumID gs = sum $ Main.id <$> filter r12g13b14 gs

split [] _ = [[]]
split (a:as) p = if p a then []:split as p else
  let (s:ss) = split as p in (a:s):ss

main :: IO ()
main = do
  s <- readFile "test_data"
  print $ sumID $ read <$> lines s
