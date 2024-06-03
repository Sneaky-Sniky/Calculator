--a Gauss eliminacio van itt megirva
--eloszor "letrehozza a haromszoget" -> megoldja az U * X = C egyenletet 
--U haromszog
--X eredmeny
--modifikalt szabadvektor ami a gauss utan jott ki

module Linearis (linearEquation) where

import Data.List (maximumBy, transpose, sortBy)
import Data.Ord (comparing)

type Matrix a = [[a]]


swapElements :: [a] -> Int -> Int -> [a]
swapElements xs i j = map snd $ sortBy (\(a, _) (b, _) -> compare a b) $ zip (indices i j) xs
  where
    indices i j = [ if k == i then j else if k == j then i else k | k <- [0..length xs - 1] ]

swapRows :: [[a]] -> Int -> Int -> [[a]]
swapRows mat i j = swapElements mat i j


gaussElim :: [[Double]] -> [Double] -> ([[Double]], [Double])
gaussElim a b = go a b 0
  where
    n = length a
    go a b i
      | i >= n = (a, b)
      | otherwise =
          let (maxValue, maxIndex) = maximumBy (comparing fst) $
                                      zip (map (\k -> abs $ (a !! k) !! i) [i..n-1]) [i..n-1]
          in if maxValue == 0
             then error "Matrix is singular"
             else
               let a' = swapRows a i maxIndex
                   b' = swapElements b i maxIndex
                   (a'', b'') = foldl (eliminate i) (a', b') [i+1..n-1]
               in go a'' b'' (i+1)
    eliminate i (a, b) j =
      let ratio = (a !! j) !! i / (a !! i) !! i
          a' = [ if r == j then zipWith (-) (a !! r) (map (* ratio) (a !! i)) else a !! r | r <- [0..n-1] ]
          b' = [ if x == j then (b !! x) - ratio * (b !! i) else b !! x | x <- [0..n-1] ]
      in (a', b')
      
uTriangleSolve :: [[Double]] -> [Double] -> [Double]
uTriangleSolve u c = reverse $ solve (reverse u) (reverse c) [] $ length c
    where
        solve [] _ x _ = x
        solve (uRow:uRows) (cVal:cVals) x i = solve uRows cVals (xVal:x) (i - 1)
            where
                xVal = (cVal - sum (zipWith (*) (drop i uRow) (x))) / (head (drop (i-1) uRow))

gaussElimSolve :: [[Double]] -> [Double]-> [Double]
gaussElimSolve a b
    | length a /= length b = error "Dimensions don't match"
    | otherwise =
        let (u, c) = gaussElim a b
            r = rank u
            n = length b
        in if r /= n
            then if any (/= 0) (drop r c)
                then error "Incompatible system"
                else error "Compatible, but indeterminate"
            else uTriangleSolve u c

rank :: [[Double]] -> Int
rank = length . takeWhile (any (/= 0))

linearEquation :: IO ()
linearEquation = do
    putStrLn "Enter the first matrix (format: [[1,2],[3,4]]):"
    matStr1 <- getLine
    let mat1 = read matStr1 :: Matrix Double
    putStrLn "Enter the vector:"
    vect <- getLine
    let vect1 = read vect :: [Double]
    let x = gaussElimSolve mat1 vect1
    print x