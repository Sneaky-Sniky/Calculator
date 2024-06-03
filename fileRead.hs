module FileRead (readFileOperation, resetFileOperation) where

import System.IO
import GHC.IO.Handle (hDuplicateTo)
import GHC.IO.Handle (hDuplicate)


readFileOperation = do
    originalStdin <- hDuplicate stdin           --kimentjuk az eredeti kimenetet egy valtozoba, hogy tudjuk visszaallitani
    fileHandle <- openFile "input.txt" ReadMode --kinyissuk read mode-ba
    hDuplicateTo fileHandle stdin               --atallitjuk a standard bemenetet egy filera
    return (originalStdin, fileHandle)          --visszateritjuk, hogy majd tudjuk visszaallitani


resetFileOperation originalStdin fileHandle = do    --ez a fuggveny allitja vissza a eredeti standard bemenetet, de kell a file, hogy csokjuk le, ezert adtuk at a fileHandle-ben
    hDuplicateTo originalStdin stdin                --atallitjuk a standard bemenetet
    hClose fileHandle
    hClose originalStdin                            --mivel mar visszaallitottuk, erre nem lesz szukseg, szoval lezarjuk

--amikor ezt hivjuk meg, szukseges a bemeneti file vegre egy 0-t rakni, hogy lezaruljon, maskepp errort ad
--pelda:
-- 1
-- 3 - 4 + 12
-- 0
