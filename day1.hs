import Data.Char

sumCalibrationVals :: String -> Int
sumCalibrationVals s = sum $ (\s -> s `seq` read [head s]*10 + read [last s]) . filter isDigit <$> lines s
