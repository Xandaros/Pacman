module Model
    ( Cell(..)
    , Row
    , Board
    , initialBoard
    ) where

import qualified Data.Vector as V

data Cell = EmptyCell
          | PacDot
          | PowerPellet
          | Wall
    deriving (Show)

type Row = V.Vector Cell
type Board = V.Vector Row

initialBoard :: Board
initialBoard = V.fromList [ V.replicate 5 Wall
                          , V.concat [V.singleton Wall, V.replicate 1 PacDot, V.singleton EmptyCell, V.singleton PowerPellet, V.singleton Wall]
                          , V.replicate 5 Wall
                          ]
