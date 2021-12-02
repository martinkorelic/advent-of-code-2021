import System.IO  
import Control.Monad

main = do  
        let list = []
        handle <- openFile "input" ReadMode
        contents <- hGetContents handle
        let singlewords = words contents
            list = f singlewords
        
        -- Part 1 
        writeFile "output_1" (show $ dive list (0,0))
        -- Part 2
        writeFile "output_2" (show $ dive2 list (0,0,0))
        hClose handle

-- Reading function
f :: [String] -> [(String, Int)]
f (l1:l2:list) = (l1, read l2): f list 
f _  = []

-- Dive
dive :: [(String, Int)] -> (Int, Int) -> Int
dive [] (dep, hor) = dep*hor
dive ((di,num):ys) (dep, hor)
    | di == "forward" = dive ys (dep, hor+num)
    | di == "down" = dive ys (dep+num, hor) 
    | otherwise = dive ys (dep-num, hor)

-- Dive2
dive2 :: [(String, Int)] -> (Int, Int, Int) -> Int
dive2 [] (dep, hor, _) = dep*hor
dive2 ((di,num):ys) (dep, hor, aim)
    | di == "forward" = dive2 ys (dep+(aim*num), hor+num, aim)
    | di == "down" = dive2 ys (dep, hor, aim+num)
    | otherwise = dive2 ys (dep, hor, aim-num)