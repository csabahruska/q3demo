{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.UI.SDL
import Graphics.UI.SDL.Mixer

import Q3Demo.Graphics.GLBackend

---------------------------
-- Temp for shader parser test

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Char8
import Data.Char
import Data.List
import Q3Demo.Data.Material
import Q3Demo.Loader.ShaderParser
import Q3Demo.Loader.Zip
import System.Directory
import System.FilePath
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Trie as T

loadArchive :: IO Archive
loadArchive = concat <$> (mapM readArchive =<< filter (\n -> ".pk3" == (map toLower $ takeExtension n)) <$> getDirectoryContents ".")

shaderMap :: Archive -> T.Trie CommonAttrs
shaderMap ar = T.fromList $ concat [eval n $ parse shaders d | (n,d) <- l]
  where
    l = [(n,decompress e) | e <- ar, let n = eFilePath e, ".shader" == takeExtension n, isPrefixOf "scripts" n]
    eval n f = case f of
        Done "" r   -> r
        Done rem r  -> error $ show (n,"Input is not consumed", rem, map fst r)
        Fail _ c _  -> error $ show (n,"Fail",c)
        Partial f'  -> eval n (f' "")
---------------------------
{-
  todo:
    select level from a list
        play level

    tech todo
        render level
        make menu

    hints:
        collect pk3 files - done
        build shader db - done
        build map db
            levelshot
            bsp file name
            data from arena script
        build character db
            icon
            ..
-}
main :: IO ()
main = do
    ar <- loadArchive
    print $ T.keys $ shaderMap ar
    tryOpenAudio 44100 AudioS16Sys 2 4096
    screen <- setVideoMode 0 0 0 [OpenGL,Fullscreen]
    setCaption "Q3 Demo" ""
    enableUnicode True
    loop (return ())
    closeAudio

loop :: IO () -> IO ()
loop display = do
    display
    event <- waitEvent
    case event of
        Quit -> return ()
        KeyDown (Keysym _ _ 'q') -> return ()
        KeyDown (Keysym SDLK_ESCAPE _ _) -> return ()
        _ -> loop display
