module Main where

import System.FilePath ((</>), splitFileName)
import System.Directory (renameFile, renameDirectory, doesFileExist, doesDirectoryExist, getDirectoryContents)
import Data.List (isPrefixOf)
import System.Environment (getArgs)
import Control.Monad (unless)
import Data.Functor ((<$>))

data WhatsIt = IsFile | IsDirectory | IsOther
  deriving (Eq)

whatsIt :: FilePath -> IO WhatsIt
whatsIt f = do
  isFile <- doesFileExist f
  if isFile then return IsFile
  else do
    isDir <- doesDirectoryExist f
    if isDir then return IsDirectory
    else return IsOther

-- depth-first post-order directory traversal
walk :: (FilePath -> IO ()) -> (FilePath -> IO ()) -> FilePath -> IO ()
walk onFile onDirectory file = do
  it <- whatsIt file
  case it of
    IsFile -> onFile file
    IsDirectory -> do
      contents <- getDirectoryContents file
      mapM_ (walk onFile onDirectory) $ map (file </>) $ filter (not . isPrefixOf ".") contents
      onDirectory file
    IsOther -> return ()

-- only process a file if the sanitized target doesn't already exist
process :: (FilePath -> FilePath -> IO ()) -> FilePath -> IO ()
process doIt file = do
  targetExists <- (/=) IsOther <$> whatsIt newFile
  unless targetExists (doIt file newFile)
  where
    newFile = sanitize file

processFile :: FilePath -> IO ()
processFile f = process renameFile f

processDir :: FilePath -> IO ()
processDir f = process renameDirectory f

dryRun :: FilePath -> FilePath -> IO ()
dryRun file newFile = putStrLn $ file ++ " -> " ++ newFile

processDryRun :: FilePath -> IO ()
processDryRun f = process dryRun f

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
  | c `elem` ['0' .. '9'] ++ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ".-_" = c
  | otherwise = '_'

main =
  getArgs >>= mapM_ (walk processFile processDir)
  -- getArgs >>= mapM_ (walk processDryRun processDryRun)
