-- download only photos from the camera that aren't already in the current directory

module Main where

import System.Directory
import System.Process
import Data.List
import Control.Monad

onlyCameraImages :: [String] -> [String]
onlyCameraImages = filter (isPrefixOf "#")

onlyImages :: [String] -> [String]
onlyImages = filter (\i -> (isSuffixOf ".JPG" i) || (isSuffixOf ".CR2" i))

main = do
  onDisk <- fmap onlyImages $ getDirectoryContents "."
  onCam <- fmap (map (take 2 . words) . onlyCameraImages . lines) $ readProcess "gphoto2" ["-L"] []
  let want = filter (\x -> not $ (head $ tail x) `elem` onDisk) onCam
  unless (null want) $
    callProcess "gphoto2" ["-p", (intercalate "," $ map tail $ map head want)]
