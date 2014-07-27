module ConnectFour where

import qualified Data.Sequence as DS
import Data.Foldable (toList)
import Data.List (transpose, isInfixOf, any)

data Color = Red | Blue deriving (Show, Read, Eq)
data Piece = Piece Color deriving (Read, Show, Eq)

type Square = Maybe Piece
type Board = DS.Seq [Square]

setupBoard :: Int -> Int -> Board
setupBoard columns rows =
  DS.replicate columns $ replicate rows Nothing

showSquare :: Square -> Char
showSquare s
  | s == Just (Piece Red) = 'r'
  | s == Just (Piece Blue) = 'b'
  | otherwise = '-'

showBoard :: Board -> [Char]
showBoard b = unlines $ map (map showSquare) (transpose bList)
  where bList = toList b

printBoard :: Board -> IO ()
printBoard board = putStrLn $ showBoard board

insertPiece :: Piece -> [Square] -> [Square]
insertPiece p [] = []
insertPiece p (Nothing:[]) = [Just p]
insertPiece p row@(top:mid:rest)
  | top == Just (Piece Red) = row
  | top == Just (Piece Blue) = row
  | top == Nothing, mid == Just (Piece Red) = [Just p, mid] ++ rest
  | top == Nothing, mid == Just (Piece Blue) = [Just p, mid] ++ rest
  | otherwise = [top] ++ insertPiece p (mid:rest)

playerMove :: Board -> Color -> Int -> Board
playerMove board color i = DS.update i newColumn board
  where newColumn = insertPiece (Piece color) (DS.index board i)

playerHasWon :: Board -> Color -> Bool
playerHasWon b c = (check b c) || (checkTransposed b c)

check :: Board -> Color -> Bool
check board color = any (connectedFour color) (toList board)

checkTransposed :: Board -> Color -> Bool
checkTransposed board color = any (connectedFour color) (transpose $ toList board)

connectedFour :: Color -> [Square] -> Bool
connectedFour col = isInfixOf (replicate 4 (Just (Piece col)))
