import System.IO  
import Control.Monad
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

main = do  
        let list = []
        handle <- openFile "input" ReadMode
        
        contents <- hGetContents handle
        let numbers = f2 (f contents)

        -- Part 1 
        writeFile "output_1" (show $ lantern numbers 80)
        -- Part 2
        writeFile "output_2" (show $ lantern numbers 256)
        hClose handle

-- Parsing functions
f :: String -> [Int]
f = map read . splitOn ","

f2 :: [Int] -> Map Int Int
f2 = foldr (\m -> Map.insertWith (+) m 1) Map.empty

lantern :: Map Int Int -> Int -> Int
lantern fish 0 = Map.foldr (+) 0 fish
lantern fish day = lantern (newday fish) (day-1)
    where
        newfishes = fromMaybe 0 $ Map.lookup 0 fish
        newday = Map.insertWith (+) 8 newfishes . Map.mapKeysWith (+) (\x -> if x == 0 then 6 else x-1)