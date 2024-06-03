module PolynomialRoots(solveQuadratic, readPolynomial) where

data QuadraticPolynomial = QuadraticPolynomial Double Double Double

solveQuadratic :: QuadraticPolynomial -> (Double, Double)
solveQuadratic (QuadraticPolynomial a b c)
    | delta < 0 = error "No real roots"
    | delta == 0 = (root, root)
    | otherwise = (root1, root2)
    where
        delta = b * b - 4 * a * c
        root = -b / (2 * a)
        root1 = (-b + sqrt delta) / (2 * a)
        root2 = (-b - sqrt delta) / (2 * a)

readPolynomial :: IO ()
readPolynomial = do
    putStrLn "Enter the coefficients (a, b, c) of the quadratic polynomial:"
    putStrLn "a:"
    a <- readLn
    putStrLn "b:"
    b <- readLn
    putStrLn "c:"
    c <- readLn
    let polynomial = QuadraticPolynomial a b c
        (root1, root2) = solveQuadratic polynomial
    putStrLn $ "Root 1: " ++ show root1
    putStrLn $ "Root 2: " ++ show root2
