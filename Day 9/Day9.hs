import System.IO
import Control.Monad
import Data.List
import Data.List.Split (splitOn)
import Data.Array (array, Array)
import Data.Char (intToDigit, digitToInt)
import Data.Maybe

main = do
        let list = []
        handle <- openFile "input" ReadMode

        contents <- hGetContents handle
        let input = f2 $ words contents

        -- Part 1
        writeFile "output_1" (show $ lavaTubes input [] input)
        -- Part 2
        input <- buildInput "input"
        let lp = lowPoints input
        writeFile "output_2" (show $ product . take 3 . reverse . sort $ map (\x -> length (buildBasin [x] input)) lp)

        hClose handle

f :: [String] -> Array (Int, Int) Int
f s = array ((0, 0), (y_max-1, x_max-1)) [ (ij, get ij) | i <- [0..(y_max-1)], j <- [0..(x_max-1)],let ij = (i,j)]
    where
        y_max = length s
        x_max = length $ head s
        get (i,j) = digitToInt $ (s !! i) !! j

f2 :: [String] -> [((Int,Int), Int)]
f2 s = [ (ij, get ij) | i <- [0..(y_max-1)], j <- [0..(x_max-1)],let ij = (i,j)]
    where
        y_max = length s
        x_max = length $ head s
        get (i,j) = digitToInt $ (s !! i) !! j

lavaTubes :: [((Int,Int), Int)] -> [Int] -> [((Int,Int), Int)] -> Int
lavaTubes ter lows [] = foldr (\x acc -> x+acc+1) 0 lows
lavaTubes ter lows (((i,j),h):ps) = lavaTubes ter (low ++ lows) ps
    where
        isLowest = foldr (\x a -> let d = fromMaybe (-1) x in if d == (-1) then a else a && (d > h)) True [ lookup (ix,jy) ter | ix <- [i-1..i+1], jy <- [j-1..j+1], (i,j) /= (ix,jy) && (ix == i || jy == j)]
        low = [h | isLowest]

buildInput :: FilePath -> IO [[Int]]
buildInput filename = map (map digitToInt) . lines <$> readFile filename :: IO [[Int]]

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x : xs)
  | x `elem` xs = removeDuplicates xs
  | otherwise = x : removeDuplicates xs

isLeastNeighbour :: [[Int]] -> (Int, Int) -> Bool
isLeastNeighbour ls (i, j) =
  let current = ls !! i !! j
      neighbours =
        [ ls !! i' !! j'
          | i' <- [i - 1, i, i + 1],
            j' <- [j - 1, j, j + 1],
            i' >= 0 && i' < length ls,
            j' >= 0 && j' < length (head ls),
            i' /= i || j' /= j,
            i' /= i - 1 || j' /= j - 1, -- do not consider diagonals
            i' /= i - 1 || j' /= j + 1,
            i' /= i + 1 || j' /= j - 1,
            i' /= i + 1 || j' /= j + 1
        ]
   in all (> current) neighbours

lowPoints :: [[Int]] -> [(Int, Int)]
lowPoints input =
  [ (i, j)
    | i <- [0 .. length input - 1],
      j <- [0 .. length (head input) - 1],
      isLeastNeighbour input (i, j)
  ]

buildBasin :: [(Int, Int)] -> [[Int]] -> [(Int, Int)]
buildBasin basin lake = do
  let newPoints =
        [ (i', j')
          | (i, j) <- basin,
            i' <- [i - 1, i, i + 1],
            j' <- [j - 1, j, j + 1],
            i' >= 0 && i' < length lake,
            j' >= 0 && j' < length (head lake),
            (i', j') `notElem` basin,
            i' /= i - 1 || j' /= j - 1, -- do not consider diagonals
            i' /= i - 1 || j' /= j + 1,
            i' /= i + 1 || j' /= j - 1,
            i' /= i + 1 || j' /= j + 1,
            lake !! i !! j <= lake !! i' !! j' && lake !! i' !! j' /= 9
        ]
  if null newPoints
    then basin
    else buildBasin (basin ++ removeDuplicates newPoints) lake