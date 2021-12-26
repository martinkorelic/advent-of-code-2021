import System.IO
import Control.Monad
import Data.List (sort)
main = do
        let list = []
        handle <- openFile "input" ReadMode

        contents <- hGetContents handle
        let input = words contents

        -- Part 1
        writeFile "output_1" (show $ syntaxScoring True input)
        -- Part 2
        writeFile "output_2" (show $ syntaxScoring False input)

        hClose handle

syntaxScoring :: Bool -> [String] -> Int
syntaxScoring p l = if p then sum $ scores p else scores2 !! (length scores2 `div` 2)
    where
        scores i = map (\y -> scoreS i (drop 1 y) (take 1 y)) l
        scores2 = sort $ map (\y -> scoreS p (drop 1 y) (take 1 y)) [ j | (i,j) <- zip (scores True) l, i == 0]

scoreS :: Bool -> String -> [Char] -> Int
scoreS p [] [] = 0
scoreS p (c:cs) [] = scoreS p cs [c] 
scoreS p [] b = if p then 0 else descoreS b
scoreS p (c:cs) (b:bs) = case (b, c) of
                            ('(',')') -> scoreS p cs bs
                            ('[',']') -> scoreS p cs bs
                            ('{','}') -> scoreS p cs bs
                            ('<','>') -> scoreS p cs bs
                            (_, ')') -> 3
                            (_, ']') -> 57
                            (_, '}') -> 1197
                            (_, '>') -> 25137
                            _ -> scoreS p cs (c:b:bs)

descoreS :: String -> Int
descoreS = foldr (\x a -> case x of 
                             '(' -> a*5+1
                             '[' -> a*5+2
                             '{' -> a*5+3
                             '<' -> a*5+4
                             _ -> a*5) 0 . reverse