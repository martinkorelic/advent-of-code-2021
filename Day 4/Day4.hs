import System.IO  
import Control.Monad
import Data.Char (digitToInt)
import Data.List.Split (splitOn)

main = do  
        let list = []
        handle <- openFile "input" ReadMode
        
        contents <- hGetContents handle
        let singlewords = words contents
        let numbers = f (head singlewords)
        let bingos = f3 (f2 (drop 1 singlewords))
        
        -- Part 1 
        writeFile "output_1" (show $ squid numbers bingos True)
        -- Part 2
        writeFile "output_2" (show $ squid numbers bingos False)
        hClose handle

-- Parsing functions
f :: [Char] -> [Int]
f = map read . splitOn ","

f2 :: [String] -> [[(Bool, Int)]]
f2 [] = []
f2 l = map (\x -> (False, read x)) (take 5 l) : f2 (drop 5 l)

f3 :: [[(Bool, Int)]] -> [[[(Bool, Int)]]]
f3 [] = []
f3 l = take 5 l : f3 (drop 5 l)

squid :: [Int] -> [[[(Bool, Int)]]] -> Bool -> Int
squid [] bing winOnly = 0
squid (n:ns) bing winOnly = let bingo = map (check n) bing
                                winners = [ x | x <- bingo, (if winOnly then id else not) $ hasWon x (replicate 5 True)]
                                boards = if winOnly then (bingo, winners) else (winners, bingo)
                            in if length winners == fromEnum winOnly then win (concat $ head $ snd boards) else squid ns (fst boards) winOnly
                            where win l = sum (map snd $ filter (not . fst) l) * n
                        
check :: Int -> [[(Bool, Int)]] -> [[(Bool, Int)]]
check i = map (map (\(x, y) -> if not x then (y==i, y) else (x,y)))

hasWon :: [[(Bool, Int)]] -> [Bool] -> Bool
hasWon [] cols = or cols
hasWon (b:bs) cols = checkRow b || hasWon bs ncols
    where
        checkRow = foldr (\x a -> a && fst x) True
        ncols = zipWith (&&) (map fst b) cols