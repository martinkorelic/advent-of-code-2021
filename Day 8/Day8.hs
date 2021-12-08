import System.IO  
import Control.Monad
import Data.List.Split (splitOn)
import Data.List (sort, isSubsequenceOf, intersect, span, partition, (\\))

main = do  
        let list = []
        handle <- openFile "input" ReadMode
        
        contents <- hGetContents handle
        let sig = f contents

        -- Part 1
        writeFile "output_1" (show $ signals1 sig)
        -- Part 2
        writeFile "output_2" (show $ signals2 sig)

        hClose handle


f :: String -> Signals
f = map (\x -> let (a,b) = break (== "|") $ words x in (a, drop 1 b)) . splitOn "\n"

type Signals = [([String],[String])]

easyDigitLen :: [Int]
easyDigitLen = [2,4,3,7]

hardDigitLen :: [Int]
hardDigitLen = [5,6]

signals1 :: Signals -> Int
signals1 = foldr (\x acc -> let y = snd x in acc + length (filter (\z -> elem (length z) easyDigitLen) y)) 0

--signals2 :: Signals -> Int
--signals2 = foldr (\(seg, out) acc -> ) 0
signals2 :: Signals -> Int
signals2 sigs = foldr (\(seg, out) acc -> let 
                                        (d1478, hD) = partition (\z -> elem (length z) easyDigitLen) seg
                                        (d235, d069) = partition (\z -> length z == 5) hD
                                        assocs = decode d1478 d069 d235
                                        in digitalise assocs out + acc
                                        ) 0 sigs
        where
            digitalise :: [(Char,String)] -> [String] -> Int
            digitalise as out = let dg = dropWhile (== '0') $ map (fnum as) out in if null dg then 0 else read dg 
            fnum as n = fst $ head $ filter (\(x,y) -> length y == length n && null (y \\ n)) as

decode :: [String] -> [String] -> [String] -> [(Char, String)]
decode d1478 d069 d235 = a1478 ++ a069 ++ a235
    where
        a1478 = decode1478 d1478
        get1 = snd $ head $ filter (\(x,y) -> x=='1') a1478
        get4 = snd $ head $ filter (\(x,y) -> x=='4') a1478
        a069 = decode069 get1 get4 d069
        a235 = decode235 get1 get4 d235

decode1478 :: [String] -> [(Char, String)]
decode1478 = map (\l -> case length l of
                 2 -> ('1', l)
                 4 -> ('4', l)
                 3 -> ('7', l)
                 _ -> ('8', l))

decode069 :: String -> String -> [String] -> [(Char, String)]
decode069 one four list = [('6', get6), ('0', head get0), ('9', head get9)]
    where
        get6 = head $ filter (\x -> 1 == length (one `intersect` x)) list
        (get0, get9) = partition (\x -> 3 == length (four `intersect` x)) (filter (/=get6) list)

decode235 :: String -> String -> [String] -> [(Char, String)]
decode235 one four list = [('2', head get2),('3', get3), ('5', head get5)] 
    where
        get3 = head $ filter ( \x -> 2 == length (one `intersect` x)) list
        (get5, get2) = partition (\x -> 3 == length (four `intersect` x)) (filter (/=get3) list)