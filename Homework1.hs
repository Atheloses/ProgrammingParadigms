import Data.List
import Data.Maybe

type Result = [String]

pp :: Result -> IO ()
pp x = putStr (concat (map (++ "\n") x))

sampleInput =
  [ "*********",
    "*s*   * *",
    "* * * * *",
    "* * * * *",
    "*   *   *",
    "******* *",
    "        *",
    "*********"
  ]

getPos :: Result -> Char -> (Int, Int)
getPos x elem = getPos' 1 0 x elem
  where
    getPos' row 0 (x : xs) elem = getPos' (row + 1) (getPosIn x elem) xs elem
    getPos' row column _ _ = (row -1, column)

getPosIn :: String -> Char -> Int
getPosIn x elem = (fromMaybe (-1) $ elemIndex elem x) + 1

getAt :: Result -> (Int, Int) -> Char
getAt x (row, column) = x !! (row -1) !! (column -1)

setAt :: Result -> (Int, Int) -> Char -> Result
setAt input (row, column) element = before ++ [(setAtIn (input !! (row -1)) (column -1) element)] ++ after
  where
    (before, _ : after) = splitAt (row -1) input

setAtIn :: [a] -> Int -> a -> [a]
setAtIn xs index element = before ++ [element] ++ after
  where
    (before, _ : after) = splitAt index xs

move :: Result -> Char -> Result
move input direction = move' input direction
  where
    move' input 'd' = goToRelative input (1, 0)
    move' input 'r' = goToRelative input (0, 1)
    move' input 'l' = goToRelative input (0, -1)
    move' input 'u' = goToRelative input (-1, 0)
    move' input _ = input

goToRelative :: Result -> (Int, Int) -> Result
goToRelative input (relRow, relCol) = goTo input (row, col) (relRow + row, relCol + col)
  where
    (row, col) = getPos input 's'

goTo :: Result -> (Int, Int) -> (Int, Int) -> Result
goTo input from to = if' (canGoTo input ' ' to) (setAt (setAt input to (getAt input from)) from ' ') input

canGoTo :: Result -> Char -> (Int, Int) -> Bool
canGoTo input elem (row, column) = (row > 0 && column > 0 && row <= (length input) && column <= length (input !! (row -1)) && (getAt input (row, column)) == elem)

maze :: Result -> String -> Result
maze input (x : xs) = maze (move input x) xs
maze input [] = input

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y