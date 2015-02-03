{-# LANGUAGE OverloadedStrings #-}

module MediaType
( MediaType (..)
, mediaType
, destination
) where

import Prelude hiding (FilePath)
import Turtle

import qualified Data.Map  as M
import qualified Data.Text as T

data MediaType = Document
               | Image
               | Music
               | Other
               deriving (Eq, Show)

mediaType :: FilePath -> MediaType
mediaType fpath =
  case extension fpath of
    (Just ext) -> M.findWithDefault Other ext fileExtMap
    _          -> Other

destination :: MediaType -> IO FilePath
destination media = do
  homeDir <- home
  case media of
    Document -> return $ homeDir </> "Documents"
    Image    -> return $ homeDir </> "Pictures"
    Music    -> return $ homeDir </> "Music"
    Other    -> return "."


-- private functions

docExts, imageExts, musicExts :: [T.Text]
docExts   = ["pdf", "doc", "md", "odf", "txt", "html"]
imageExts = ["gif", "png", "jpg", "jpeg"]
musicExts = ["mp3", "flac", "m4a"]

fileExtMap :: M.Map T.Text MediaType
fileExtMap =
  let tuple exts ftype = flip map exts $ \ext -> (ext,ftype)
  in  M.fromList $  tuple docExts   Document
                 ++ tuple imageExts Image
                 ++ tuple musicExts Music
