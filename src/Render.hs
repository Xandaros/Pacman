module Render
    ( renderGame
    , initRender
    ) where

import           Prelude hiding ((.), id)
import           Control.Category
import           Control.Monad
import           Debug.Trace

import           Graphics.DrawingCombinators as Draw
import           Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Data.Vector as V

import           Model

resolution :: (Int, Int)
resolution = (640, 480)

initRender :: IO GLFW.Window
initRender = do
    True <- GLFW.init -- TODO: unsafe
    Just win <- GLFW.createWindow -- TODO: unsafe
        (fromIntegral $ fst resolution)
        (fromIntegral $ snd resolution) 
        "Pacman"
        Nothing
        Nothing

    GLFW.makeContextCurrent (Just win)

    return win

renderGame :: Board -> GLFW.Window -> IO ()
renderGame board window = do
    clearRender (drawBoard board) >> GLFW.swapBuffers window

-- TODO: deduplicate
drawBoard :: Board -> Image Any
drawBoard board =
    let len = V.length board
        size = 2/fromIntegral len
        minPoint = (-1) + (size/2)
        maxPoint = 1 - (size/2)
    in  mconcat $ forLoop (\idx row ->
        let t = (fromIntegral idx / fromIntegral (len-1))
        in  (Draw.translate (0, interpolate minPoint maxPoint t)
          %% Draw.scale 1 (size/2)
          %% drawRow row)) (V.toList board)

drawRow :: Row -> Image Any
drawRow row =
    let len = V.length row
        size = 2/fromIntegral len
        minPoint = (-1) + (size/2)
        maxPoint = 1 - (size/2)
    in  mconcat $ forLoop (\idx cell ->
        let t = (fromIntegral idx / fromIntegral (len-1))
        in  (Draw.translate (interpolate minPoint maxPoint t, 0)
          %% Draw.scale (size/2) 1
          %% drawCell cell)) (V.toList row)

drawCell :: Cell -> Image Any
drawCell cell = case cell of
    EmptyCell -> mempty
    PacDot -> Draw.scale 0.1 0.1 %% circle
    PowerPellet -> Draw.scale 0.3 0.3 %% circle
    Wall -> tint (Draw.Color 0.0625 0.09375 0.5 1)
         $  convexPoly [(-1,-1), (1,-1), (1, 1), (-1, 1)]

-- UTIL

forLoop :: (Int -> a -> b) -> [a] -> [b]
forLoop f xs' = forLoop' xs' 0
    where
        forLoop' [] _ = []
        forLoop' (x:xs) idx = f idx x : forLoop' xs (idx+1)

interpolate :: (Num a) => a -> a -> a -> a
interpolate min max t = min + (max-min)*t
