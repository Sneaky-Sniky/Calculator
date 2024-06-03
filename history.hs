--Ebben a fileban lehet kezelni a szamologep elozmeny file-jat
--Ket dolgot tudunk tenni:
--                          - megtekinteni az elozmenyeket
--                          - torli a tartalmat

module History (     --letrehozok egy importot HistoryHandler neven, amiben 2 fuggveny van
    clearFile,
    displayFileContent
) where

import System.IO ( hClose, openFile, IOMode(WriteMode) )

clearFile :: FilePath -> IO ()
clearFile path = do
    handle <- openFile path WriteMode --ha irasba nyissuk meg, leuritit a filet
    hClose handle

displayFileContent :: FilePath -> IO ()
displayFileContent filePath = do
    contents <- readFile filePath
    putStrLn contents

