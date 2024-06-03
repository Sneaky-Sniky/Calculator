module PolynomSimplification (executeSimplification) where
import Data.Char (isDigit, isSpace)
import Data.List (groupBy, sortOn)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)


splitPolynomial :: String -> [String]
splitPolynomial s = splitTerms s 
splitTerms :: String -> [String]
splitTerms [] = []                      --itt lesz a felbontott lista
splitTerms s
    | head s == '+' || head s == '-' = let (term, rest) = break (`elem` "+-") (tail s) -- + vagy - szerinti felbontas
                                       in (head s : term) : splitTerms rest --az elsot megcsinalja 1 sorral fenebb majd rekurzivan hivja a vagast
    | otherwise = let (term, rest) = break (`elem` "+-") s --az elso szamnak nincs feltetlenul +- ezert kell kulon lekezeles
                  in term : splitTerms rest --meghivja a felso ft rekurzivisan

trim :: String -> String
trim = f . f                --leveszi az elejerol es a vegerol a spacet
   where f = reverse . dropWhile isSpace

parseTerm :: String -> (Int, Int)
parseTerm term =
    let (coeffStr, expStr) = break ('x' ==) term --ez x-ek szerint lebontja, es beteszi egy tuple-be, peldaul ha 2x^4-ent kap, akkor ebbol lesz (2,4)
        coeff = if null coeffStr || coeffStr == "+" then 1 else if coeffStr == "-" then -1 else fromMaybe 1 (readMaybe (dropWhile(== '+') (takeWhile (/= 'x') coeffStr)) :: Maybe Int) --ez leszedi az egyutthatot, ha nincs egyutthatoja, 1-est vagy -1-et tesz
                                                                                                                                                                                        --ha van egyutthato, akkor beteszi azt, az atalakito ismeri a "-" jelt de a "+"-t mnem, ezert kellett a dropWhile
                                                                                                                                                                                        --es a takeWhile azt csinalja, hogy x-ig megy
        exp = if null expStr then 0 else if expStr == "x" then 1  else fromMaybe 1 (readMaybe (tail $ dropWhile (/= '^') expStr) :: Maybe Int)      --hasolo a felsohoz, negativ kitevoju hatvagyokat nem tud kezelni, csak pozitiv
    in (coeff, exp)

combineTerms :: [(Int, Int)] -> [(Int, Int)]
combineTerms = map sumCoeffs . groupBy (\(_, e1) (_, e2) -> e1 == e2) . sortOn snd  -- rendezi"second" azaz kitevo szerint, majd mappel azokat a egyutthatokat adja ossze, ahol a hatvany megegyezik
    where sumCoeffs terms = (sum (map fst terms), snd (head terms))     --kmeghatarozza, hogy a sumCoeffs mivel foglalkozzon, ahol "terms"-t ir

formatTerm :: (Int, Int) -> String --egy darab szamnak a formazasa, hogy visszakeruljon erre az alakra, pl: -2x^3
formatTerm (coeff, 0) = show coeff
formatTerm (coeff, 1) = showCoeff coeff ++ "x"
    where showCoeff 1 = "+"
          showCoeff (-1) = "-"
          showCoeff c = if c > 0 then "+" ++ show c else show c
formatTerm (coeff, exp) = showCoeff coeff ++ "x^" ++ show exp
    where showCoeff 1 = "+"
          showCoeff (-1) = "-"
          showCoeff c = if c > 0 then "+" ++ show c else show c

executeSimplification :: IO ()
executeSimplification = do
    input <- getLine    --beolvassa a polinomot
    let terms = map trim $ splitPolynomial input --feldarabolja
    let parsedTerms = map parseTerm terms --tupleekbe helyezi a listat, majd listat csinal beloluk
    let combinedTerms = combineTerms parsedTerms --osszevonja oket
    let formattedTerms = map formatTerm combinedTerms --osszevonas utan formazza oket
    let result = concat formattedTerms --osszeteszi lista helyett stringe
    print result --kiirja a stringet
    
