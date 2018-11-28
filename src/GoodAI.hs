module GoodAI
    (
        GoodAI(..),
        determineAIMove,
        getBoardValue,
        getPlayerBoardValue
    ) where

import GameBoard
import GameLogic
import qualified Data.Array.Repa as R
import Player
import Data.List

-- A record that contains the attributes of the AI

-- depth: How deep the AI will traverse
-- insight: How much the AI will prefer earlier results to later ones
-- playerNum: The player number that the AI is controlling
data GoodAI = GoodAI {depth :: Int, insight :: Double, playerNum :: Player}

-- The high-level function for determining what move the AI should take
determineAIMove :: GameBoard -> GoodAI -> Int
determineAIMove board ai =
    let 
        player = (playerNum ai) -- The "player" the AI is controlling
        nextPlayer = (player + 1) `mod` 2 -- The player that will go next
        possibleMoves = [0, 6, 5, 1, 4, 2, 3] -- The places we can put spots, ordered by favor
        doableMoves = [x | x <- possibleMoves, (moveFits board x)] -- The moves that we can actually put on the board
        possibleBoards = zip doableMoves (map (\x -> place board player x) doableMoves) -- The possible endstates
        -- Note: The possible boards are zipped together with the moves to reach them
        -- So that the AI can figure out what move to take to get to that board
        curDepth = (depth ai) -- The AI's depth value
        detBoardVal = (\b -> -- The function for determining the value of a board
            let boardVal = getBoardValue b player -- We do a precursory static evaluation
            in  if boardVal > 200 then 9999999 -- If our static evaluation is big then it's a win
                else (if boardVal < -200 then -9999999 -- If it's small then it's a loss
                else negate (getDeepBoardValue b ai (curDepth - 1) nextPlayer) * (insight ai))) -- Otherwise, dig down
        -- The below code basically does the following:
        --    Takes all of the possible actions and determines their resulting boards' values through DFS
        --    Finds the highest value board we can go to
        --    Returns the action to go to that board
    in fst $ maximumBy (\a b -> compare (snd a) (snd b)) (map (\(x,y) -> (x, detBoardVal y)) possibleBoards)

-- A function which does a depth-first tree traversal to determine the overall value of a branch
getDeepBoardValue :: GameBoard -> GoodAI -> Int -> Player -> Double
getDeepBoardValue board ai curDepth player 
    | curDepth == 0 = currentBoardVal -- If we're at the end of our depth, use the static value of the board
    | currentBoardVal > 200 = currentBoardVal -- If the static value of the board is a victory, use that
    | currentBoardVal < -200 = currentBoardVal -- If the static value of the board is a loss, use that
    | otherwise = maximum $ map detBoardVal possibleBoards -- Otherwise, find the highest value of the leaves
    where 
        -- Thse values are similar to the one used for the determineAIMove function
        detBoardVal = (\b -> negate (getDeepBoardValue b ai (curDepth - 1) nextPlayer) * (insight ai))
        possibleMoves = [0, 6, 5, 1, 4, 2, 3]
        currentBoardVal = fromIntegral $ getBoardValue board player
        possibleBoards = [(place board player x) | x <- possibleMoves, (moveFits board x)]
        nextPlayer = (player + 1) `mod` 2

-- Static board analysis
getBoardValue :: GameBoard -> Player -> Int
getBoardValue board player =
    -- This gets the individual board analysis for each player and combines them
    let players = [0, 1]
    -- A function that negates players that aren't the currently acting player
        method = (\p -> 
            if p == player
                then (getPlayerBoardValue board p)
                else negate (getPlayerBoardValue board p)
            )
    -- Combine the values
    in foldr (+) 0 (map method players)

-- The individual board analysis function
-- This condenses the board into an integer by adding all of its values
-- This doesn't account for enemy pieces as that's done higher up
-- For details on how the chaining algorithm works, take a quick look at GameLogic.hs
getPlayerBoardValue :: GameBoard -> Player -> Int
getPlayerBoardValue board player = R.foldAllS (\acc v -> (chainVal v) + acc) 0 (findLines board player)

-- The function to be applied to each individual chain value
-- Essentially, for values 4 and over it's 200, otherwise it's the value squared
chainVal :: Int -> Int
chainVal val
    | val >= 4 = 200
    | otherwise = val * val