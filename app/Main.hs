module Main where

import JsonParser

import System.Environment
import System.IO (
  Handle,
  IOMode(ReadMode),
  withFile,
  hGetContents
  )

useFile :: (String -> IO()) -> Handle -> IO()
useFile = \func handle -> do
  text <- hGetContents handle
  func text

main :: IO()
main = do
  (fileName:_) <- getArgs
  withFile fileName ReadMode (useFile print)
