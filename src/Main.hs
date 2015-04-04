module Main
    ( main
    ) where

import Control.Concurrent

import Graphics.Rendering.OpenGL

import Render
import Logic
import Model

main :: IO ()
main = do
    window <- initRender

    loop window
    where
        loop window = do
            renderGame initialBoard window
            threadDelay 1000000
            loop window
