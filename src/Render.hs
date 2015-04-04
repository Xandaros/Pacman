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
    clearRender (drawGame board) >> GLFW.swapBuffers window

drawGame :: Board -> Image Any
drawGame = mconcat . V.toList . V.map drawRow

drawRow :: Row -> Image Any
drawRow row =
    let len = V.length row
        size = 2/fromIntegral len
        minPoint = (-1) + (size/2)
        maxPoint = 1 - (size/2)
        fullSize = maxPoint - minPoint
    in  mconcat $ forLoop (\idx cell ->
        let t = (fromIntegral idx / fromIntegral (len-1))
        --in  Draw.translate (minPoint + fullSize*t, 0) %% point (0,0)) [(), (), (), (), ()]
        in  (Draw.translate (minPoint + fullSize*t, 0) %% Draw.scale (size/2) 1 %% drawCell cell)) (V.toList row)

drawCell :: Cell -> Image Any
drawCell _ = tint (Draw.Color 1 0 0 1) $ circle

forLoop :: (Int -> a -> b) -> [a] -> [b]
forLoop f xs' = forLoop' xs' 0
    where
        forLoop' [] _ = []
        forLoop' (x:xs) idx = f idx x : forLoop' xs (idx+1)
