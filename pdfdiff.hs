module Main where

import System.Process
import System.Exit
import Control.Monad
import System.Environment
import Text.Printf

diffFileName = "delta"
diffFileExt = ".png"



-- Compare the given page of the two given pdf files and produce a diff file
-- called "deltaXXX.png" where XXX is the 0-padded page number.
comparePage :: FilePath -> FilePath -> Int -> IO ()
comparePage file1 file2 pageNr = do
  -- compare returns a value between 0 and 1 if images differ. This is not a
  -- failure, but readProcess throws an exception.
  (exitCode,_,_) <- readProcessWithExitCode "compare"
    ["-alpha", "off"
    ,"-metric", "mae"
    , formatFilePage file1 pageNr
    , formatFilePage file2 pageNr
    , diffFileName ++ printf "%03d" pageNr ++ diffFileExt
    ]
    ""
  case exitCode of
    ExitSuccess -> return ()
    ExitFailure 2 -> putStrLn $ "error comparing page " ++ show pageNr
    ExitFailure n -> putStrLn $ "page " ++ show pageNr ++ " differs"
  where
  formatFilePage fileName pageNr = fileName ++ "[" ++ show pageNr ++ "]"



-- determine number of pages of a pdf file
numPdfPages :: FilePath -> IO Int
numPdfPages fileName = do
  out <- readProcess "pdfinfo" [fileName] ""
  return $ numPages out
  where
  -- parse output of pdfinfo and determine number of pages
  numPages :: String -> Int
  numPages pdfinfoOutput = numPages
    where
    numPages = findNumPages $ words pdfinfoOutput
    findNumPages [] = 0
    findNumPages ("Pages:" : numPages : _) = read numPages
    findNumPages (_:rest) = findNumPages rest



main = do
  [file1, file2] <- getArgs
  numPages1 <- numPdfPages file1
  numPages2 <- numPdfPages file2
  let numPages = minimum [numPages1, numPages2]
  sequence_
    [ comparePage file1 file2 pageNr
    | pageNr <- [0..(numPages - 1)]
    ] 
