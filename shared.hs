module Shared (
    lexer,
    parens,
    integer,
    float,
    reservedOp,
    reserved,
    whiteSpace,
    table,
    evaluate,
    saveToFile
) where

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

lexer = Tok.makeTokenParser emptyDef        --letrejon egy "felvago" fuggveny, ami meg ures, nincs szabaly, ami szerint meg felvagjon

--ezek a szabelyok leheetnek
parens = Tok.parens lexer   --zarojel szerint vag fel
integer = Tok.integer lexer --egesz szamok szerint
float = Tok.float lexer --valos szamok szerint
reservedOp = Tok.reservedOp lexer --operatorok szerint vag fel
reserved = Tok.reserved  --szo alapjan vag fel (fuggveny neveket, pl sin, cos..stb)
whiteSpace = Tok.whiteSpace lexer --feherkarakterek szerint




table = [ [prefix "-" negate, prefix "+" id ] --fel van sorolva, milyen jel mit csinal
        , [binary "^" (**) AssocRight]
        , [postfix "++" (+1), postfix "--" (subtract 1) ]
        , [binary "*" (*) AssocLeft, binary "/" (/) AssocLeft]
        , [binary "+" (+) AssocLeft, binary "-" (-) AssocLeft]
        ]

--seged fuggvenyei a table-nek
binary name fun assoc = Infix (do{ reservedOp name; return fun }) assoc
prefix name fun       = Prefix (do{ reservedOp name; return fun })
postfix name fun      = Postfix (do{ reservedOp name; return fun })


historyFile :: FilePath
historyFile = "calculator_history.txt"


expr :: Parser Double
expr = buildExpressionParser table term --csinal meg egy vagot, ami a muveleteket tudja feldolgozni, pl ha adok neki egy 1+2+3-at az expr-ben 6 lesz

term = parens expr --ez meghatarozza, hogy mi az, hogyan dolgozza fel ez alapjan
    <|> try float
    <|> fmap fromInteger integer --lekezeli magatol a try-t
    <|> try trigFunctions
    <|> logFunctions    --utolso opcio, szoval ide sem kell

trigFunctions :: Parser Double
trigFunctions = do
    func <- choice (map string ["sin", "cos", "tan", "asin", "acos", "atan"]) --megmondja, milyen fuggvenyek kozott lehet valasztani
    whiteSpace
    val <- parens expr --ide kerul az ertek, amit kell szamoljon
    return $ case func of
        "sin"  -> sin val
        "cos"  -> cos val
        "tan"  -> tan val
        "asin" -> asin val
        "acos" -> acos val
        "atan" -> atan val
        _      -> error "Unknown function"

logFunctions :: Parser Double
logFunctions = do
    func <- choice (map string ["log", "ln", "exp", "sqrt"])
    whiteSpace
    val <- parens expr
    return $ case func of
        "log"  -> logBase 10 val
        "ln"   -> log val
        "exp"  -> exp val
        "sqrt" -> sqrt val
        _      -> error "Unknown function"


evaluate :: String -> Either ParseError Double
evaluate input = parse (do { whiteSpace; x <- expr; eof; return x }) "" input --ez a fuggveny hivja meg az inputra a feldolgozast

saveToFile :: String -> IO ()
saveToFile content = appendFile historyFile (content ++ "\n") -- appendel hivja meg, hogy ne torolje ki a file tartalmat, beteszi az eredmenyt es az egyenletet, utana egy entert tesz