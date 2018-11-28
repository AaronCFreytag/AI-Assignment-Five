module Main where

import Lib
import GameBoard
import GameLogic
import qualified Data.Array.Repa as R
import qualified Data.Vector.Unboxed as V
import System.Console.ANSI
import Text.Read
import Data.Maybe
import Data.List
import GoodAI

data Option a = Option {req :: (String -> Option a -> Bool), commandDisplay :: String, name :: String, branch :: (String -> IO a)}

main :: IO ()
main = do
    clearScreen
    showWelcome
    menuLoop

menuLoop :: IO()
menuLoop = do
    let options = [
            Option (\s o -> s == (commandDisplay o)) "V" "Versus" doVersus :: Option (),
            Option (\s o -> s == (commandDisplay o)) "A" "Fight AI" doAIControlled :: Option (),
            Option (\s o -> s == (commandDisplay o)) "T" "Test Something" doSomeTest :: Option ()
            ] :: [Option ()]
    showOptions options
    putStrLn ""
    userInput <- getLine
    let selectedOption = find (\o -> (req o) userInput o) options :: Maybe (Option ())
    maybe (putStrLn "Error: Invalid command!") (\o -> (branch o) "") selectedOption
    menuLoop

showWelcome :: IO ()
showWelcome = do
    putStrLn $ makeTitle 60 "CONNECT 4"

doVersus :: String -> IO ()
doVersus call = do
    clearScreen
    let board = makeEmptyBoard 7 6
    showFancyBoard board
    gameLoop board [playerMove, playerMove] 0

gameLoop :: GameBoard -> [GameBoard -> Int -> IO (Maybe Int)] -> Int -> IO ()
gameLoop board playerActs player = do
    let nextPlayer = (player + 1) `mod` (length playerActs)
    putStrLn ("Player " ++ show (player + 1) ++ " Turn")
    afterBoard <- takeTurn board player (playerActs !! player)
    if (hasWon afterBoard player)
        then do
            putStrLn ("Player " ++ show (player + 1) ++ " Wins!")
        else
            gameLoop afterBoard playerActs nextPlayer

takeTurn :: GameBoard -> Int -> (GameBoard -> Int -> IO (Maybe Int)) -> IO (GameBoard)
takeTurn board player act = do
    actResult <- getValidAct board player act
    let newBoard = makeMove board player actResult
    case newBoard of
        Right board -> do
            clearScreen
            showFancyBoard board
            putStrLn ("Player " ++ show (player + 1) ++ " placed a piece in Column " ++ show actResult)
            putStrLn ""
            return board
        Left err -> do
            putStrLn ("Error: " ++ reason err)
            takeTurn board player act

getValidAct ::  GameBoard -> Int -> (GameBoard -> Int -> IO (Maybe Int)) -> IO (Int)
getValidAct board player act = do
    actResult <- (act board player)
    case actResult of
        Just pos -> return pos
        Nothing -> do
            putStrLn "Invalid Command!"
            getValidAct board player act

doAIControlled :: String -> IO ()
doAIControlled call = do
    putStrLn "Do you want to go first or second? (1 or 2)"
    opt <- askUntilFirstOrSecond
    clearScreen
    let board = makeEmptyBoard 7 6
    showFancyBoard board
    if opt == 1
        then gameLoop board [playerMove, aiMove] 0
        else gameLoop board [aiMove, playerMove] 0

playerMove :: GameBoard -> Int -> IO (Maybe Int)
playerMove board player = do
    let options = [
            Option (\s o -> not (readMove s == Nothing)) "[0-6]" "Place Piece" putPiece :: Option (Int),
            Option (\s o -> s == (commandDisplay o)) "A" "Let The Computer Choose" (letAIMove board player) :: Option (Int)
            ] :: [Option (Int)]
    showOptions options
    putStrLn ""
    userInput <- getLine
    let selectedOption = find (\o -> (req o) userInput o) options :: Maybe (Option Int)
    maybe (return Nothing) (\o -> Just <$> ((branch o) userInput)) selectedOption

putPiece :: String -> IO (Int)
putPiece call = return $ fromMaybe 0 (readMove call)

readMove :: String -> Maybe Int
readMove str = 
    let val = readMaybe str :: Maybe Int
    in case val of
        Just v -> if (v < 7 && v >= 0) then val else Nothing
        Nothing -> Nothing

askUntilFirstOrSecond :: IO Int
askUntilFirstOrSecond = do
    str <- getLine
    let val = readFirstOrSecond str
    case val of
        Just v -> return v
        Nothing -> askUntilFirstOrSecond

readFirstOrSecond :: String -> Maybe Int
readFirstOrSecond str =
    let val = readMaybe str :: Maybe Int
    in case val of
        Just v -> if (v == 1 || v == 2) then val else Nothing
        Nothing -> Nothing

letAIMove :: GameBoard -> Int -> String -> IO (Int)
letAIMove board player call = fromMaybe <$> return 0 <*> (aiMove board player)

aiMove :: GameBoard -> Int -> IO (Maybe Int)
aiMove board player = do
    let ai = GoodAI 5 0.7 player
    return $ Just (determineAIMove board ai)

doSomeTest :: String -> IO ()
doSomeTest call = do
    let testList = [0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,2,2,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0]
    let board = makeFromList 7 6 testList
    showBoard $ findLines board 0
    putStrLn $ show (hasWon board 0)

doExit :: IO ()
doExit = do
    putStrLn "See you next time!"

makeTitle :: Int -> String -> String
makeTitle width text =
    let surrounding = replicate width '='
        innerWidth = width - 4
        body = "||" ++ (pad text innerWidth) ++  "||"
    in unlines [surrounding, body, surrounding]

showOptions :: [Option a] -> IO [()]
showOptions options = mapM (\o -> putStrLn $ (commandDisplay o) ++ ": " ++ (name o)) options

pad :: String -> Int -> String
pad text width =
    let empty = (width - (length text)) `div` 2
        oddOffset = (width - (length text)) `mod` 2
    in (replicate empty ' ') ++ text ++ (replicate (empty + oddOffset) ' ')

showBoard :: (V.Unbox a, Show a) => Board a -> IO ()
showBoard board = putStrLn $ makeBoardText board

showFancyBoard :: Board Int -> IO ()
showFancyBoard board = putStrLn $ makeFancyBoardText board

makeBoardText :: (V.Unbox a, Show a) => Board a -> String
makeBoardText board = 
    let space = 3
        displayWidth = ((width board) * space) + 4
        displayHeight = (height board) + 2
        surrounding = replicate displayWidth '='
        rows = map (\i -> row board i) [0..((height board) - 1)]
        rowText = map (\row -> R.foldAllS (\i acc -> i ++ acc) "||" (R.map (\j -> pad (show j) space) (row)) ++ "||") rows
    in unlines ([surrounding] ++ rowText ++ [surrounding])

makeFancyBoardText :: Board Int -> String
makeFancyBoardText board = 
    let space = 3
        displayWidth = ((width board) * space) + 4
        displayHeight = (height board) + 2
        numberMarkers = "===0==1==2==3==4==5==6==="
        surrounding = replicate displayWidth '='
        rows = map (\i -> row board i) [0..((height board) - 1)]
        rowText = map (\row -> R.foldAllS (\i acc -> i ++ acc) "||" (R.map (\j -> pad (boardPieceDisplay j) space) (row)) ++ "||") rows
    in unlines ([numberMarkers] ++ [surrounding] ++ rowText ++ [surrounding])

boardPieceDisplay :: Int -> String
boardPieceDisplay val
    | val == 1 = "#"
    | val == 2 = "O"
    | otherwise = " "