--ez a fo file, ami kezeli a szamologepet
--minden fuggveny mas fileba van megirva az atlathatosag kedveert, majd importolva van ide
--ezt kell elinditani, majd futtatni
--szamologep meghivasasa: main 


import History
import BasicOperations
import Advanced
import MatrixOperations
import PolynomialRoots
import FileRead
import PolynomSimplification
import Linearis
import System.IO
import System.Directory (removeFile, doesFileExist)
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Tok
import Data.List
import Data.Maybe
import Data.Char (isDigit)
import Control.Arrow ((&&&))


selectCalculatorFunction :: IO ()                                   --opcio kezelo fuggveny, legeloszor kiirja az opciokat a kelhasznalo szamara
selectCalculatorFunction = do 
    putStrLn "Choose from the following calculator functions:"
    putStrLn "1. Basic Operations"
    putStrLn "2. Advanced Operations"
    putStrLn "3. Simplify an Equation"
    putStrLn "4. Operations with Matrices"
    putStrLn "5. Calculating Roots of Polynomials"
    putStrLn "6. Solving Linear Equations"
    putStrLn "7. Import from File"
    putStrLn "8. View Previous Calculations"
    putStrLn "9. Delete Calculation History"
    putStrLn "Select a function (1-9) or press '0' to exit:"
    input <- getLine                                           --beolvassa a szamot, amit a felhasznalo valaszt
    case input of
        "0"  -> putStrLn "Exiting."
        "1"  -> putStrLn "Basic Operations function selected." >> executeBasicOperations >> selectCalculatorFunction                                --kiirja, mit valasztott a felhasznalo, majd meghivja a fuggvenyt/fuggvenyeket. a >> operator csak annyit jelent, hogy csinald ezt -> utana ezt -> utana ezt
        "2"  -> putStrLn "Advanced Operations function selected." >> executeAdvancedOperations >> selectCalculatorFunction
        "3"  -> putStrLn "Simplify an Equation function selected." >> executeSimplification >> selectCalculatorFunction
        "4"  -> putStrLn "Operations with Matrices function selected." >> executeMatrixOperations >> selectCalculatorFunction
        "5"  -> putStrLn "Calculating Roots of Polynomials function selected." >> readPolynomial >> selectCalculatorFunction
        "6"  -> putStrLn "Solving Linear Equations function selected." >> linearEquation >> selectCalculatorFunction
        "7"  -> do 
            putStrLn "Import from File function selected."
            (originalStdin, fileHandle)<- readFileOperation
            selectCalculatorFunction
            resetFileOperation originalStdin fileHandle
            selectCalculatorFunction
        "8"  -> putStrLn "View history function selected." >> displayFileContent "calculator_history.txt" >> selectCalculatorFunction
        "9"  -> putStrLn "Delete Calculation History function selected." >> clearFile "calculator_history.txt" >> selectCalculatorFunction
        _    -> putStrLn "Invalid input. Please try again." >> selectCalculatorFunction                                                            --minden mas esetben kiirja, hogy rossz az input

main :: IO ()
main = selectCalculatorFunction