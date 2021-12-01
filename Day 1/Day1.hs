import System.IO  
import Control.Monad

main = do  
        let list = []
        handle <- openFile "input" ReadMode
        contents <- hGetContents handle
        let singlewords = words contents
            list = f singlewords
        -- Part 1 
        writeFile "output_1" (show $ sonarSweep' list)
        -- Part 2
        writeFile "output_2" (show $ sonarSweep'' list [])
        hClose handle
        
f :: [String] -> [Int]
f = map read

sonarSweep' :: [Int] -> Int
sonarSweep' list = sum $ zipWith (\x y -> fromEnum (x > y)) (drop 1 list) list

sonarSweep'' :: [Int] -> [Int] -> Int
sonarSweep'' (y1:y2:y3:ys) sums = sonarSweep'' (y2:y3:ys) (sums ++ [y1+y2+y3])
sonarSweep'' _ sums = sonarSweep' sums

-- Extra
sonarSweepZ :: [Int] -> Int -> Int
sonarSweepZ [] c = c
sonarSweepZ [_] c = c
sonarSweepZ (y1:y2:ys) c
    | y2 > y1 = sonarSweepZ (y2:ys) c+1
    | otherwise = sonarSweepZ (y2:ys) c