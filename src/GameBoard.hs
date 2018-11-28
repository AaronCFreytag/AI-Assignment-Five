module GameBoard
    (
        GameBoard,
        Piece,
        IndexBoard,
        Board,
        makeEmptyBoard,
        makeIndexBoard,
        makeFromList,
        column,
        row,
        width,
        height,
        get,
        place,
        getHighestEmpty
    ) where

import Player
import Data.Array.Repa
import qualified Data.Vector.Unboxed as V

type Position = Int
type Piece = Player
type Board = Array U DIM2
type GameBoard = Array U DIM2 (Piece)
type IndexBoard = Array U DIM2 ((Int, Int))
type Row = Array D DIM1
type Column = Array D DIM1

makeEmptyBoard :: Int -> Int -> GameBoard
makeEmptyBoard width height = fromListUnboxed (Z :. width :. height) (replicate (width * height) (0::Piece))

makeIndexBoard :: Int -> Int -> IndexBoard
makeIndexBoard width height = fromListUnboxed (Z :. width :. height) ((,) <$> [0..(width - 1)] <*> [0..(height - 1)])

makeFromList :: Int -> Int -> [Piece] -> GameBoard
makeFromList width height list = fromListUnboxed (Z :. width :. height) list

column :: (V.Unbox a) => Board a -> Int -> Column a
column board index = slice board (Any :. (index) :. All)

row :: (V.Unbox a) => Board a -> Int -> Row a
row board index = slice board (Any :. (index))

width :: (V.Unbox a) => Board a -> Int
width board = listOfShape (extent board) !! 1

height :: (V.Unbox a) => Board a -> Int
height board = listOfShape (extent board) !! 0

get :: (V.Unbox a) => Board a -> Int -> Int -> a
get board x y = index board (Z :. x :. y)

place :: GameBoard -> Piece -> Position -> GameBoard
place board piece position =
    let splitLoc = toIndex (extent board) (Z :. position :. (getHighestEmpty board position))
        splitVec = V.splitAt splitLoc (toUnboxed board)
        left = fst splitVec
        middle = V.singleton (piece + 1)
        right = V.tail (snd splitVec)
    in fromUnboxed (extent board) (left V.++ middle V.++ right)

getHighestEmpty :: GameBoard -> Position -> Int
getHighestEmpty board position = foldAllS (\x y -> if y == 0 then x + 1 else x) (-1) (column board position)
