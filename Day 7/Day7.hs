import System.IO  
import Control.Monad
import Data.List.Split (splitOn)
import Data.List (sort)

main = do  
        let list = []
        handle <- openFile "input" ReadMode
        
        contents <- hGetContents handle
        let numbers = f contents

        -- Part 1
        let median = findMedian numbers
        writeFile "output_1" (show $ sum [abs (x - median) | x <- numbers])
        -- Part 2
        let mean = truncate (findMean numbers) -- round down
        writeFile "output_2" (show $ sum [nthTriangleNumber $ abs (x - mean) | x <- numbers])
        hClose handle

-- Parsing functions
f :: String -> [Int]
f = map read . splitOn ","

findMedian :: [Int] -> Int
findMedian ls = sort ls !! (length ls `div` 2)

findMean :: (Fractional a) => [Int] -> a
findMean ls = (fromIntegral . sum) ls / (fromIntegral . length) ls

nthTriangleNumber :: Int -> Int
nthTriangleNumber n = n * (n + 1) `div` 2

{-
-- Utility function
getMaxBound :: Bounded a => a -> a
getMaxBound _ = maxBound
crabs :: Bool -> [Int] -> [Int] -> Int -> Int -> Int -> Int
crabs _ _ [] _ _ cheap = cheap
crabs p hor (ps:pos) i cum cheap
    | cum > cheap = crabs p hor pos 0 0 cheap
    | i >= length hor = crabs p hor pos 0 0 $ min cum cheap
    | otherwise = crabs p hor pos (i+1) (cum + (if p then dif1 else dif2)) cheap
        where
            dif1 = abs $ (hor !! i) - ps
            dif2 = sum [1..dif1]

average :: [Int] -> Int
average xs = sum xs `div` length xs

crabs' :: Bool -> [Int] -> Int
crabs' p list = foldr (\x acc -> sums x + acc) 0 $ map (\x -> abs (x-avg)) list
    where 
        avg = average list
        sums x = if p then x else sum [1..x]
-}