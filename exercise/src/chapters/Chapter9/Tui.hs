module Tui where

import Control.Concurrent (threadDelay)

sleepSecond :: Int -> IO ()
sleepSecond n = do
    threadDelay (n * 1000000)

cls :: IO ()
cls = putStr "\ESC[2J"

--

type Pos = (Int, Int)
type Board = [Pos]

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO ()
writeat p xs = do
    goto p
    putStr xs

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (a:as) = do
    a
    seqn as

readLine :: String -> IO String
readLine xs = do
    c <- getChar
    case c of
        '\n' -> return xs
        '\DEL' -> do
            if xs == ""
            then do
                putStr "\ESC[1D\ESC[1D"
                putStr "  "
                putStr "\ESC[1D\ESC[1D"
                readLine xs
            else do
                putStr "\ESC[1D\ESC[1D\ESC[1D"
                putStr "   "
                putStr "\ESC[1D\ESC[1D\ESC[1D"
                readLine (init xs)
        _ -> do
            readLine (xs ++ [c])