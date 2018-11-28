module GameLogic
    (
        makeMove,
        findLines,
        moveFits,
        hasWon,
        MoveError,
        reason
    ) where

import GameBoard
import Player
import qualified Data.Array.Repa as R

data MoveError = MoveError {reason :: String} deriving (Show)
type Move = Int

-- Most of this stuff is boring, so the comments will be sparse
-- There will be more comments in the area where the passes are being applied
-- To find chains

-- Makes a move if it's valid
makeMove :: GameBoard -> Player -> Move -> Either MoveError GameBoard
makeMove prevBoard player move
    | not $ moveInBounds prevBoard move = Left MoveError {reason = "Move out of bounds"}
    | not $ moveFits prevBoard move = Left MoveError {reason = "Column is full"}
    | otherwise = Right (addPiece prevBoard player move)

-- Checks if a move is in bounds
moveInBounds :: GameBoard -> Move -> Bool
moveInBounds board move = move >= 0 && move < width board

-- Checks if a move fits (i.e. the column isn't full)
moveFits :: GameBoard -> Move -> Bool
moveFits board move = (get board move 0) == 0 

-- Puts a piece on a board; Kind of just an abstraction function
addPiece :: GameBoard -> Piece -> Move -> GameBoard
addPiece board piece move = place board piece move

-- Has the player won?
-- Simply checks if there's a value 4 or higher in the chain function
hasWon :: GameBoard -> Player -> Bool
hasWon board player = (R.foldAllS max 0 (findLines board player)) >= 4

-- CHAIN FUNCTION
-- This takes a board and determines how many chains that belong to a particular player there are
-- This works in two passes:
--    A pass which checks in the direction and increments in one direction, resulting in one value representing the
--      length of the entire chain.
--    A pass which propogates the value in the reverse direction to cover the entire chain.
--
-- This is done for each direction, and then combined with the maximum values.
findLines :: GameBoard -> Player -> GameBoard
findLines board player = 
    let indices = makeIndexBoard (width board) (height board)
        hPass = hLinePass indices board 4 (player + 1)
        vPass = vLinePass indices board 4 (player + 1)
        ddPass = ddLinePass indices board 4 (player + 1)
        duPass = duLinePass indices board 4 (player + 1)
    in foldr (\a b -> R.computeUnboxedS (R.zipWith max a b)) hPass [vPass, ddPass, duPass]

-- Returns a board with all 1s where all the player's pieces are, 0s everywhere else
numberOneBoard :: GameBoard -> Player -> GameBoard
numberOneBoard board player = R.computeUnboxedS $ R.map (\i -> if i == player then 1 else 0) board

-- Peforms a direction pass and a propogation pass in the horizontal direction
hLinePass :: IndexBoard -> GameBoard -> Int -> Player -> GameBoard
hLinePass ib board l player = multiPropPass ib (multiDirPass ib board (-1, 0) l player) (-1, 0) l player

-- Performs a direction pass and a propogation pass in the vertical direction
vLinePass :: IndexBoard -> GameBoard -> Int -> Player -> GameBoard
vLinePass ib board l player = multiPropPass ib (multiDirPass ib board (0, -1) l player) (0, -1) l player

-- Performs a direction pass and a propogation pass in the diagonal down left direction
ddLinePass :: IndexBoard -> GameBoard -> Int -> Player -> GameBoard
ddLinePass ib board l player = multiPropPass ib (multiDirPass ib board (-1, 1) l player) (-1, 1) l player

-- Performs a direction pass and a propogation pass in the diagonal up left direction
duLinePass :: IndexBoard -> GameBoard -> Int -> Player -> GameBoard
duLinePass ib board l player = multiPropPass ib (multiDirPass ib board (-1, -1) l player) (-1, -1) l player

-- Performs a directional pass using an arbitrary "direction" offset
-- This basically takes the values and adds to itself if values are connected to it
-- This repeats for as many times as we want to go deep
multiDirPass :: IndexBoard -> GameBoard -> (Int, Int) -> Int -> Player -> GameBoard
multiDirPass ib board off length player
    | length == 1 = numberOneBoard board player
    | otherwise = dirPass ib (multiDirPass ib board off (length - 1) player) off (length - 1)
    
-- Performs a propogation pass using an arbitrary "direction" offset
-- This replicates the values which are collected for the chain
-- Once again, repeats for as many times as we go deep
multiPropPass :: IndexBoard -> GameBoard -> (Int, Int) -> Int -> Player -> GameBoard
multiPropPass ib board off length player
    | length == 1 = board
    | otherwise = propPass ib (multiPropPass ib board off (length - 1) player) off (length)

-- A single directional pass
-- Basically just maps the array to the single tile dir pass function
dirPass :: IndexBoard -> GameBoard -> (Int, Int) -> Int -> GameBoard
dirPass ib board off layer = R.computeUnboxedS $ R.map (\c -> singleDirPass c off board layer) ib

-- What happens on a tile level for the directional pass
-- Any connected like values will increment one of them by 1
-- Also does some bounds checking
singleDirPass :: (Int, Int) -> (Int, Int) -> GameBoard -> Int -> Int
singleDirPass (x, y) (xDif, yDif) board layer
    | initVal < layer = initVal
    | checkX < 0 || checkX >= (width board) = initVal
    | checkY < 0 || checkY >= (height board) = initVal
    | not $ checkVal == initVal = initVal
    | otherwise = initVal + 1
    where
        initVal = get board x y
        checkVal = get board checkX checkY
        checkX = x + xDif
        checkY = y + yDif

-- A single propogation pass
-- Once again basically just maps the single value propogation pass across an array
propPass :: IndexBoard -> GameBoard -> (Int, Int) -> Int -> GameBoard
propPass ib board off layer = R.computeUnboxedS $ R.map (\c -> singlePropPass c off board layer) ib

-- What happens on a tile level for propogation pass
-- Replicated values to any numbers that are lower, sort of
-- Also does some bounds checking
singlePropPass :: (Int, Int) -> (Int, Int) -> GameBoard -> Int -> Int
singlePropPass (x, y) (xDif, yDif) board layer
    | checkX < 0 || checkX >= (width board) = initVal
    | checkY < 0 || checkY >= (height board) = initVal
    | checkVal < layer = initVal
    | checkVal > initVal = checkVal
    | otherwise = initVal
    where
        initVal = get board x y
        checkVal = get board checkX checkY
        checkX = x - xDif
        checkY = y - yDif