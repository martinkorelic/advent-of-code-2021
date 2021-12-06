import System.IO  
import Control.Monad
import Data.List.Split (splitOn)
import Data.List ( groupBy )
import Data.Map (Map)
import qualified Data.Map as Map

main = do  
        let list = []
        handle <- openFile "input" ReadMode
        
        contents <- hGetContents handle
        let singlewords = words contents
        let numbers = f2 (f singlewords)

        -- Part 1 
        writeFile "output_1" (show $ venture False numbers Map.empty)
        -- Part 2
        writeFile "output_2" (show $ venture True numbers Map.empty)
        hClose handle

-- Parsing functions
f :: [String] -> [(Int, Int)]
f l = map (\x -> let line = map read $ splitOn "," x in (head line, last line)) (filter (/= "->") l)

f2 :: [a] -> [[a]]
f2 (l1:l2:ls) = [l1,l2] : f2 ls
f2 _ = []

venture :: Bool -> [[(Int, Int)]] -> Map (Int,Int) Int -> Int
venture part2 [] mask = Map.foldr (\x acc -> if x > 1 then acc+1 else acc) 0 mask
venture part2 (l:ls) mask
    | x1 == x2 = venture part2 ls (check mask [(x1,z) | z <- distance y1 y2])
    | y1 == y2 = venture part2 ls (check mask [(z,y1) | z <- distance x1 x2])
    | part2 = venture part2 ls (check mask diagonals)
    | otherwise = venture part2 ls mask
    where
        start = head l
        end = last l
        x1 = fst start
        y1 = snd start
        x2 = fst end
        y2 = snd end
        distance s e = if e <= s then [e..s] else [s..e]
        check ml = foldr (\m -> Map.insertWith (+) m 1) ml
        diagonals = case (x1<x2, y1<y2) of 
                        (True, False) -> zip (reverse $ distance x1 x2) (distance y1 y2)
                        (False, True) -> zip (distance x1 x2) (reverse $ distance y1 y2)
                        _ -> zip (distance x1 x2) (distance y1 y2)