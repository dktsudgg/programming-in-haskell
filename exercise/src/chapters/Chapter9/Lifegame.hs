module Lifegame
    (
    ) where

import Parsing -- if you run in ghci, then you have to do ":set -isrc/chapters/Chapter8"
import Control.Concurrent (threadDelay)
import System.Console.ANSI

sleepSecond :: Int -> IO ()
sleepSecond n = do threadDelay (n * 1000000)

cls :: IO ()
cls = putStr "\ESC[2J"

--

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

--

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (a:as) = do a
                 seqn as

-- 

width :: Int
width = 5

height :: Int
height = 5

type Board = [Pos]

glider :: Board
glider = [(4,2), (2,3), (4,3), (3,4), (4,4)]

showcells :: Board -> IO ()
showcells b = seqn [writeat p "0" | p <- b]

clscells :: Board -> Board -> IO ()
clscells asIsB toBeB = seqn [writeat p " " | p <- asIsB, notElem p toBeB]

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b
isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (elem p b)

neighbs :: Pos -> [Pos]
neighbs (x, y) = map wrap [(x-1, y-1),  (x, y-1),   (x+1, y-1),
                           (x-1, y),                (x+1, y),
                           (x-1, y+1),  (x, y+1),   (x+1, y+1)]

wrap :: Pos -> Pos
wrap (x, y) = ((x - 1) `mod` width + 1,
               (y - 1) `mod` height + 1)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2, 3]]

births :: Board -> [Pos]
births b = [p | p <- rmdups (concat (map neighbs b)),
                isEmpty b p,
                liveneighbs b p == 3]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

life :: Board -> IO ()
life b = do showcells b
            sleepSecond 3
            clscells b (nextgen b)
            life (nextgen b)

wait :: Int -> IO ()
wait n = seqn [return () | _ <- [1..n]]

startLifeGame :: IO ()
startLifeGame = do b <- initGlider []
                   cls
                   life b

initGlider :: Board -> IO Board
initGlider b = do putStr "Put an alive cell's position: "
                  str <- readLine ""
                  if str == "done" then return b
                  else do case parse str2Pos str of
                            [((num1, num2), _)] -> initGlider (b ++ [(num1, num2)])
                            _ -> error "Wrong Position !"

str2Pos :: Parser Pos
str2Pos = do symbol "("
             num1 <- natural
             symbol ","
             num2 <- natural
             symbol ")"
             return (num1, num2)

readLine :: String -> IO String
readLine xs = do c <- getChar
                 case c of
                  '\n' -> return xs
                  '\DEL' -> do if xs == ""
                               then do putStr "\ESC[1D\ESC[1D"
                                       putStr "  "
                                       putStr "\ESC[1D\ESC[1D"
                                       readLine xs
                               else do putStr "\ESC[1D\ESC[1D\ESC[1D"
                                       putStr "   "
                                       putStr "\ESC[1D\ESC[1D\ESC[1D"
                                       readLine (init xs)
                  _ -> do readLine (xs ++ [c])