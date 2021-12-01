import System.IO  
import Control.Monad

main = do  
        let list = []
        handle <- openFile "input" ReadMode
        contents <- hGetContents handle
        let singlewords = words contents
            list = f singlewords
        -- Part 1 
        writeFile "output_1" (show $ sonarSweep' list 0)
        -- Part 2
        writeFile "output_2" (show $ sonarSweep'' list [] 0)
        hClose handle
        
f :: [String] -> [Int]
f = map read

sonarSweep' :: [Int] -> Int -> Int
sonarSweep' list c = sum $ zipWith (\x y -> fromEnum (x > y)) (drop 1 list) list

sonarSweep'' :: [Int] -> [Int] -> Int -> Int
sonarSweep'' [] sums c = sonarSweep' sums c 
sonarSweep'' [_] sums c = sonarSweep' sums c
sonarSweep'' [_,_] sums c = sonarSweep' sums c
sonarSweep'' (y1:y2:y3:ys) sums c = sonarSweep'' (y2:y3:ys) (sums ++ [y1+y2+y3]) c

-- Extra
sonarSweepZ :: [Int] -> Int -> Int
sonarSweepZ [] c = c
sonarSweepZ [_] c = c
sonarSweepZ (y1:y2:ys) c
    | y2 > y1 = sonarSweepZ (y2:ys) c+1
    | otherwise = sonarSweepZ (y2:ys) c