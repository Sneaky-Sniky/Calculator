module MatrixOperations (
    addMatrices,
    multiplyMatrices,
    transposeMatrix,
    executeMatrixOperations
) where

import System.IO
import Data.List

type Matrix a = [[a]]                                       --letrehoz egy matix tipust, ami listaban a lista, mind ugyanolyan tipusu

addMatrices :: Num a => Matrix a -> Matrix a -> Maybe (Matrix a) --kell a maybe a nothing-hoz 
addMatrices mat1 mat2
    | dimensionsMatch mat1 mat2 = Just $ zipWith (zipWith (+)) mat1 mat2 --a zipWith az azt mondja meg, hogy paronkent mit csinaljon 2 matrix elemevel
    | otherwise = Nothing

multiplyMatrices :: Num a => Matrix a -> Matrix a -> Maybe (Matrix a)
multiplyMatrices mat1 mat2
    | dimensionsMatchForMultiplication mat1 mat2 = Just [[sum $ zipWith (*) row col | col <- transpose mat2] | row <- mat1] 
    | otherwise = Nothing

transposeMatrix :: Matrix a -> Matrix a
transposeMatrix = transpose --beepitett fuggveny a Data.List-ben

dimensionsMatch :: Matrix a -> Matrix a -> Bool
dimensionsMatch mat1 mat2 = length mat1 == length mat2 && all (\row -> length row == length (head mat2)) mat1 --megnezi, ha egy sorban ugyaannyi elem van, majd ha a sokor szama megegyezik 

dimensionsMatchForMultiplication :: Matrix a -> Matrix a -> Bool
dimensionsMatchForMultiplication mat1 mat2 = length (head mat1) == length mat2 --szorzasnal kulon, mert ott csak az elso oszlopszama es a masodik sora kell egyezzen

executeMatrixOperations :: IO ()
executeMatrixOperations = do
    putStrLn "Choose a matrix operation:"
    putStrLn "1. Add matrices"
    putStrLn "2. Multiply matrices"
    putStrLn "3. Transpose a matrix"
    putStrLn "Enter your choice (1-3):"
    choice <- getLine
    case choice of
        "1" -> do
            putStrLn "Enter the first matrix (format: [[1,2],[3,4]]):"
            matStr1 <- getLine
            let mat1 = read matStr1 :: Matrix Double
            putStrLn "Enter the second matrix:"
            matStr2 <- getLine
            let mat2 = read matStr2 :: Matrix Double
            case addMatrices mat1 mat2 of
                Just result -> putStrLn $ "Result of addition: " ++ show result
                Nothing -> putStrLn "Cannot add matrices with different dimensions."
        "2" -> do
            putStrLn "Enter the first matrix (format: [[1,2],[3,4]]):"
            matStr1 <- getLine
            let mat1 = read matStr1 :: Matrix Double
            putStrLn "Enter the second matrix:"
            matStr2 <- getLine
            let mat2 = read matStr2 :: Matrix Double
            case multiplyMatrices mat1 mat2 of
                Just result -> putStrLn $ "Result of multiplication: " ++ show result
                Nothing -> putStrLn "Cannot multiply matrices with incompatible dimensions."
        "3" -> do
            putStrLn "Enter the matrix to transpose (format: [[1,2],[3,4]]):"
            matStr <- getLine
            let mat = read matStr :: Matrix Double
            putStrLn $ "Transposed matrix:\n" ++ unlines (map show (transposeMatrix mat))
        _ -> putStrLn "Invalid choice. Please enter a number between 1 and 3."


