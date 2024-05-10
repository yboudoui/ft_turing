module Parser where

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
