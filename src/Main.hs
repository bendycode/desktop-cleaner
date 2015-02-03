{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import MediaType (mediaType, destination)
import Prelude hiding (FilePath)
import Turtle

main :: IO ()
main = do
  homeDir <- home
  sh $ do
    files <- ls $ homeDir </> "Desktop"
    liftIO $ moveFile files

moveFile :: FilePath -> IO ()
moveFile fpath = do
  dest <- destination $ mediaType fpath
  mv fpath dest
