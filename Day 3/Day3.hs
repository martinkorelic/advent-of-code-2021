import System.IO  
import Control.Monad
import Data.Char (digitToInt)

main = do  
        let list = []
        handle <- openFile "input" ReadMode
        contents <- hGetContents handle
        let singlewords = words contents
            list = f singlewords
        
        -- Part 1 
        writeFile "output_1" (show $ binDiagnostic list)
        -- Part 2
        writeFile "output_2" (show $ binDiagnostic2 list)
        hClose handle

-- Parsing function
f :: [String] -> [[Int]]
f = map $ map digitToInt

binDiagnostic :: [[Int]] -> Int
binDiagnostic list = bintodec gamma * bintodec epsilon
    where
        freq = getFreq list
        gamma = getRates (<) (length list) freq
        epsilon = map (\x -> if x==1 then 0 else 1) gamma

binDiagnostic2 :: [[Int]] -> Int
binDiagnostic2 list = bintodec (binDiagnosticR (<=) list 0) * bintodec (binDiagnosticR (>) list 0)

binDiagnosticR :: (Int -> Int -> Bool) -> [[Int]] -> Int -> [Int]
binDiagnosticR _ [ls] _ = ls
binDiagnosticR crit l i = binDiagnosticR crit (filter (\x -> (x !! i) == (rating !! i)) l) (i+1)
    where
        len = length l
        freq = getFreq l
        rating = getRates crit len freq

getFreq :: [[Int]] -> [Int]
getFreq list = foldr (\b acc -> zipWith (+) acc b) (replicate innerlen 0) list
    where  
        innerlen = length (head list)

getRates :: (Int -> Int -> Bool) -> Int -> [Int] -> [Int]
getRates crit len = map (\x -> if (len - x) `crit` x then 1 else 0)

bintodec :: [Int] -> Int
bintodec l = foldr (\x y -> x + 2*y) 0 (reverse l)