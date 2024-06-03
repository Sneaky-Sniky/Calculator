module Advanced (executeAdvancedOperations) where
import System.IO
import Shared
import System.Directory (removeFile, doesFileExist)


executeAdvancedOperations :: IO ()
executeAdvancedOperations = do
    putStrLn "Enter an expression with advanced operations (exponentiation, roots, trigonometric, logarithmic, exponential):"
    input <- getLine
    case evaluate input of      --meghivja az evaluate fuggvenyt a sharedbol
        Left err -> print err   --ha leftet az vissza, akkor error
        Right val -> do         --ha right akkor helyes
            print val           --kiirja az erteket
            saveToFile (input ++ " = " ++ show val)--kiirja a muveletet amire megvolt hivva (fileba) es melle mivel egyenlo