module NimGame
    (
    ) where

import Parsing -- if you run in ghci, then you have to do ":set -isrc/chapters/Chapter8"
import Tui

type StarCount = Int
type StarBoard = [StarCount]

starBoard :: StarBoard
starBoard = [5,4,3,2,1]

echoStarBoard :: StarBoard -> IO ()
echoStarBoard b = seqn
    [ do
        putStr $ show index ++ ": "
        echoStar starCnt
        putStrLn ""
    | (index, starCnt) <- zip [1..] b ]

echoStar :: StarCount -> IO ()
echoStar starCnt = seqn [putStr "*" | _ <- [1..starCnt]]

type Gamer = String
type Gamers = [Gamer]

gamers :: Gamers
gamers = ["User1", "User2"]

nextTurn :: Int -> Gamers -> Gamer
nextTurn curCycle gamers = gamers !! (curCycle `mod` length gamers)

nextBoard :: Int -> StarBoard -> StarBoard
nextBoard choice board =
    take (choice - 1) board ++ [(board !! (choice - 1)) - 1] ++ drop choice board

isEnd :: StarBoard -> Bool
isEnd = foldr (\a b -> a == 0 && b) True

startNimGame :: IO ()
startNimGame = nimGame 0 starBoard

nimGame :: Int -> StarBoard -> IO ()
nimGame curCycle board = do
    if isEnd board
    then do
        putStrLn $ nextTurn curCycle gamers ++ " lost the game.."
        return ()
    else do
        echoStarBoard board
        putStr $ nextTurn curCycle gamers ++ "'s choice: "
        numStr <- readLine ""
        do
            case parse natural numStr of
                [(num, _)]  -> nimGame (curCycle + 1) (nextBoard num board)
                _           -> do
                    putStrLn "Illegal input.. try again !"
                    nimGame curCycle board
                    return ()

