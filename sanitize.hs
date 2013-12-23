module Main where

import System.FilePath ((</>), splitFileName)
import System.Directory (renameFile, renameDirectory, doesFileExist, doesDirectoryExist, getDirectoryContents)
import Data.List (isPrefixOf)
import System.Environment (getArgs)

data WhatsIt = IsFile | IsDirectory | IsOther

whatsIt :: FilePath -> IO WhatsIt
whatsIt f = do
  isFile <- doesFileExist f
  if isFile
  then return IsFile
  else do
    isDir <- doesDirectoryExist f
    if isDir then return IsDirectory
    else return IsOther

-- depth-first post-order directory tree traversal
walk :: (FilePath -> IO ()) -> (FilePath -> IO ()) -> FilePath -> IO ()
walk onFile onDirectory curName = do
  it <- whatsIt curName
  case it of
    IsFile -> onFile curName
    IsDirectory -> do
      contents <- getDirectoryContents curName
      mapM_ (walk onFile onDirectory) $ map (curName </>) $ filter (not . isPrefixOf ".") contents
      onDirectory curName
    IsOther -> return ()

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
