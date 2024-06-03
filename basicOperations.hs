module BasicOperations (executeBasicOperations) where

import System.IO
import Shared

--basic operations ugyan az mint az adnvanced
executeBasicOperations :: IO ()
executeBasicOperations = do
    putStrLn "Enter an expression:"
    input <- getLine
    case evaluate input of
        Left err -> print err
        Right val -> do
            print val
            saveToFile (input ++ " = " ++ show val)