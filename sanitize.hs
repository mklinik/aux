module Main where

import System.FilePath ((</>), splitFileName)
import System.Directory (renameFile, renameDirectory, doesFileExist, doesDirectoryExist, getDirectoryContents)
import Data.List (isPrefixOf)
import System.Environment (getArgs)

-- depth-first post-order directory tree traversal
walk :: (FilePath -> IO ()) -> (FilePath -> IO ()) -> FilePath -> IO ()
walk onFile onDirectory curName = do
  isFile <- doesFileExist curName
  if isFile
  then onFile curName
  else do
    isDir <- doesDirectoryExist curName
    if isDir
    then do
      contents <- getDirectoryContents curName
      mapM_ (walk onFile onDirectory) $ map (curName </>) $ filter (not . isPrefixOf ".") contents
      onDirectory curName
    else return () -- neither a file nor a directory

processFileDryRun :: FilePath -> IO ()
processFileDryRun f = putStrLn $ f ++ " -> " ++ sanitize f

processFile :: FilePath -> IO ()
processFile f = renameFile f (sanitize f)

processDir :: FilePath -> IO ()
processDir f = renameDirectory f (sanitize f)

main =
  getArgs >>= mapM_ (walk processFile processDir)

-- Replace everything that is not a nice character in a filename by an underscore.
--
-- Operates on the last path component only
sanitize :: FilePath -> FilePath
sanitize f = path </> sanitizedFileName
  where
    (path, fileName) = splitFileName f
    sanitizedFileName = map sanitizeOne fileName

sanitizeOne :: Char -> Char
sanitizeOne c
  | c `elem` ['0' .. '9'] ++ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ".-_/" = c
  | otherwise = '_'
