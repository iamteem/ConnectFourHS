module ConnectFour where

import Data.List (isInfixOf)
import qualified Data.Maybe as DM
import qualified Data.Foldable as DF
import qualified Data.Sequence as DS

data Color = Red | Blue deriving (Show, Eq)
data GameState = Draw | NextMove Color | Win Color
type Coords = (Int, Int)
data Square = Square Coords (Maybe Color)
type BoardData = DS.Seq Square
type BoardSize = (Int, Int)
data Board = Board BoardData BoardSize

initializeBoard :: Board
initializeBoard = Board squares (width, height)
  where squares = DS.fromList squaresList
        width = 7
        height = 6
        squaresList = map (\coord -> Square coord Nothing) [(x, y) | x <- [1..width], y <- [1..height]]

getBoardSize :: Board -> BoardSize
getBoardSize (Board _ size) = size

boardHeight = snd . getBoardSize

getBoardData :: Board -> BoardData
getBoardData (Board boardData _) = boardData

getRow :: Board -> Int -> BoardData
getRow board row = DS.filter f (getBoardData board)
  where f square = (snd . getCoords) square == row

getColumn:: Board -> Int -> BoardData
getColumn board column = DS.filter f (getBoardData board)
  where f square = (fst . getCoords) square == column

getCoords :: Square -> Coords
getCoords (Square coords _) = coords

instance Show Board where
  show board = unlines $ map (\row -> showBoardData row)  rows
    where boardData = getBoardData board
          rowIndexes = reverse [1..(boardHeight board)]
          rows = map (getRow board) rowIndexes

instance Show Square where
  show sq
    | color == Just Red = "r"
    | color == Just Blue = "b"
    | otherwise = "-"
      where color = getSquareColor sq

getSquareColor :: Square -> Maybe Color
getSquareColor (Square _ a) = a

showBoardData :: BoardData -> String
showBoardData = DF.concatMap show

insertColor :: Board -> Color -> Int -> (Board, Maybe Coords)
insertColor board color column = (newBoard, coords)
  where boardSize = getBoardSize board
        (boardData, i) = insertColorToBoardData (getBoardData board) color column
        newBoard = Board boardData boardSize
        coords = if i == Nothing then Nothing
                                 else (Just (coordsFromIndex board (DM.fromJust i)))

insertColorToBoardData :: BoardData -> Color -> Int -> (BoardData, Maybe Int)
insertColorToBoardData boardData color column = (newBoardData, empty)
  where replaceFn sq = Square (getCoords sq) (Just color)
        empty = firstEmpty boardData column
        newBoardData = if empty == Nothing then boardData else DS.adjust replaceFn (DM.fromJust empty) boardData

firstEmpty :: BoardData -> Int -> Maybe Int
firstEmpty boardData column = DS.findIndexL emptySquare boardData
  where emptySquare sq = (Nothing == (getSquareColor sq)) && (column == fst (getCoords sq))

validInsert :: Board -> Color -> Int -> Bool
validInsert board color column = columnInBoard && columnHasFreeSquare
  where columnInBoard = column `elem` [1..(fst (getBoardSize board))]
        columnHasFreeSquare = DS.findIndexL emptySquare (getColumn board column) /= Nothing
        emptySquare square = Nothing == getSquareColor square

indexFromCoords :: Board -> Coords -> Int
indexFromCoords board coords = (y - 1) * width + x
  where width = fst $ getBoardSize board
        (x, y) = coords

coordsFromIndex :: Board -> Int -> Coords
coordsFromIndex board i = (x, y)
  where width = fst $ getBoardSize board
        y = 1 + (i `div` width)
        x = 1 + (i `mod` width)

connectedFour :: BoardData -> Color -> Bool
connectedFour bdata color = isInfixOf fourPieces blist
  where blist = map getSquareColor (DF.toList bdata)
        fourPieces = replicate 4 (Just color)

getLines :: Board -> Coords -> [BoardData]
getLines board (x, y) = [vertical, horizontal] --, diagonal1, diagonal2]
  where boardData = getBoardData board
        vertical = DS.filter (\sq -> x == fst (getCoords sq)) boardData
        horizontal = DS.filter (\sq -> y == snd (getCoords sq)) boardData

isBoardFull :: Board -> Bool
isBoardFull board = DF.all isNonEmptySquare (getBoardData board)
  where isNonEmptySquare square = Nothing /= (getSquareColor square)

evaluateBoard :: Board -> Color -> (Maybe Coords) -> GameState
evaluateBoard board color Nothing = NextMove color
evaluateBoard board color (Just coords) = if playerWin then Win color
                                           else if isBoardFull board then Draw
                                           else NextMove (getOtherColor color)
  where lines = getLines board coords
        playerWin = DF.any (\bd -> connectedFour bd color) lines

getOtherColor :: Color -> Color
getOtherColor Red = Blue
getOtherColor Blue = Red

{-TODO:-}
  {-- Fix getLines (horizontal doesn't work)-}
  {-- add diagonal1, diagonal2-}
  {-- simplify everything-}
