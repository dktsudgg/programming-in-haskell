module Tui where

import Control.Concurrent (threadDelay)

sleepSecond :: Int -> IO ()
sleepSecond n = do threadDelay (n * 1000000)

cls :: IO ()
cls = putStr "\ESC[2J"

--

type Pos = (Int, Int)
type Board = [Pos]

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (a:as) = do a
                 seqn as
