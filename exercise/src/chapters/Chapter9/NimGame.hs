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
echoStarBoard b = seqn [do putStr $ show index ++ ": "
                           echoStar starCnt
                           putStrLn ""
                        | (index, starCnt) <- zip [1..] b]

echoStar :: StarCount -> IO ()
echoStar starCnt = seqn [putStr "*" | _ <- [1..starCnt]]

startNimGame :: IO ()
startNimGame = do putStrLn "Nim game start !"
                  echoStarBoard starBoard

