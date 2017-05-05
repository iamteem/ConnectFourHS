{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Foldable as F
import Text.Read (readMaybe)
import System.Console.Haskeline


type GridSize = (Int, Int)
type Grid = Seq.Seq (Seq.Seq Cell)
data GameGrid = GameGrid { gridSize :: GridSize
                         , grid :: Grid
                         } deriving (Show)
data Cell = Cell (Maybe Disc) deriving (Eq, Show)
emptyCell = Cell Nothing

data Color = Black | White deriving (Show, Read, Eq)
data Player = Player Color deriving (Show, Eq)
data Disc = Disc Color deriving (Read, Show, Eq)
data Game = Game { gameGrid :: GameGrid
                 , activePlayer :: Player
                 } deriving (Show)

availableSizes :: [GridSize]
availableSizes = [(7, 6), (8, 7), (9, 7), (10, 7), (8, 8)]

choicesForSizes :: Map.Map Char GridSize
choicesForSizes = Map.fromList $ zip ['a'..] availableSizes

run :: IO ()
run = do
  game <- newGame
  runStateT playerTurn game
  return ()

playerTurn :: StateT Game IO ()
playerTurn = do
  game <- get
  let player = activePlayer game
      playerColor = getPlayerColor player
      ggrid = gameGrid $ game
      g = grid $ ggrid
  lift $ displayGame game
  lift $ putStrLn $ "Player " ++ show playerColor ++ "'s Turn"
  let columns = map (+1) $ allowedColumns $ g
  col <- lift $ promptForColumn columns
  case insertDisc (col - 1) playerColor g of
    Left s -> lift $ putStrLn s
    Right newGrid -> evaluateGame $ game { gameGrid = ggrid { grid = newGrid }, activePlayer = nextPlayer player }

evaluateGame :: Game -> StateT Game IO ()
evaluateGame game = do
  if noMoreMoves game
    then do lift $ displayGame game
            lift $ putStrLn "No more moves! Game is a tie."
    else
      case gridHasAWinner g of
        Nothing -> do put game
                      playerTurn
        Just player -> do lift $ displayGame game
                          lift $ putStrLn $ "Congratulations! " ++ show (getPlayerColor player) ++ " wins!"
  return ()
  where g = grid $ gameGrid $ game

nextPlayer (Player Black) = Player White
nextPlayer (Player White) = Player Black

newGame :: IO Game
newGame = do
  welcomeMessage
  size <- pickSize
  return $ Game (newGameGrid size) (Player White)

welcomeMessage :: IO ()
welcomeMessage = putStrLn "Let's play Connect Four!"

pickSize :: IO GridSize
pickSize = do promptForSize
              selection <- prompt "Choose size: "
              let c = let stripped = T.strip $ T.pack selection
                      in if T.null stripped then '-' else T.head stripped
              case Map.lookup c choicesForSizes of
                Just size -> return size
                Nothing -> pickSize

promptForSize :: IO ()
promptForSize = putStrLn $ "Please select grid size:\n" ++ choicesText
  where choicesText = unlines ls
        sizeChoice c (cols, rows) = [c] ++ ") " ++ show cols ++ "x" ++ show rows
        ls = Map.elems $ Map.mapWithKey sizeChoice choicesForSizes

newGameGrid :: GridSize -> GameGrid
newGameGrid size@(cols, rows) = GameGrid size emptyGrid
  where emptyGrid = Seq.replicate cols $ Seq.replicate rows emptyCell

showCell :: Cell -> String
showCell (Cell c) =
  case c of
    Just (Disc White) -> "w"
    Just (Disc Black) -> "b"
    Nothing -> "-"

displayGame :: Game -> IO ()
displayGame game = do
  displayGrid $ gameGrid game
  return ()

displayGrid :: GameGrid -> IO ()
displayGrid g = do putStrLn header
                   putStrLn $ showGrid $ grid $ g
  where header = concat $ L.intersperse " " $ map show $ L.take (fst $ gridSize g) [1..]

showGrid :: Grid -> String
showGrid grid = let lines = L.transpose $ grid2Strings grid
                    placeSpace = map (L.intersperse ' ')
                in unlines $ placeSpace lines

grid2Strings :: Grid -> [String]
grid2Strings grid = map (\seq -> concatMap showCell $ F.toList seq) $ F.toList grid

gridHasAWinner :: Grid -> Maybe Player
gridHasAWinner grid = let strings = grid2Strings grid
                          transposed = L.transpose strings
                          skewed = L.transpose $ skew strings
                          rSkewed = L.transpose $ skew $ map reverse strings
                          grids = transposed ++ rSkewed ++ skewed ++ strings
                      in searchWinner grids

skew :: [String] -> [String]
skew [] = []
skew (l:ls) = l : (skew $ map indent ls)
  where indent line = '*' : line

searchWinner :: [String] -> Maybe Player
searchWinner ls
  | blackWins ls = Just (Player Black)
  | whiteWins ls = Just (Player White)
  | otherwise = Nothing
  where
    blackWins = L.any $ L.isInfixOf "bbbb"
    whiteWins = L.any $ L.isInfixOf "wwww"

prompt :: String -> IO String
prompt s = runInputT defaultSettings $ prompt' s
prompt' s =
  do val <- getInputLine s
     case val of
       Nothing -> prompt' s
       Just s -> return s

promptForColumn :: [Int] -> IO Int
promptForColumn allowedColumns = do
  s <- prompt "Insert in Column: "
  case (readMaybe s :: Maybe Int) of
    Nothing -> promptAgain
    Just val -> if val `elem` allowedColumns then return val else promptAgain
  where promptAgain =
          do putStrLn "Invalid Column!"
             promptForColumn allowedColumns

noMoreMoves :: Game -> Bool
noMoreMoves game = L.null $ allowedColumns $ grid $ gameGrid game

allowedColumns :: Grid -> [Int]
allowedColumns g = let heads = F.toList $ Seq.mapWithIndex head g
                       head i col = let c Seq.:< _ = Seq.viewl col in (i, c)
                       isEmptyCell (_, cell) = cell == emptyCell
                   in map fst $ filter isEmptyCell heads

insertDisc :: Int -> Color -> Grid -> Either String Grid
insertDisc col color grid
  | col `elem` (allowedColumns grid) = Right $ insertDisc' col color grid
  | otherwise = Left "Unable to insert disc"

insertDisc' :: Int -> Color -> Grid -> Grid
insertDisc' index color grid =
  let colSeq = Seq.index grid index
      lastNothingCellIndex = Seq.findIndexR (== emptyCell) colSeq
      disc = Just $ Disc color
      newCol = case lastNothingCellIndex of
                 Just i -> Seq.update i (Cell disc) colSeq
                 Nothing -> colSeq
  in Seq.update index newCol grid

getPlayerColor (Player col) = col

-- Test grids e.g. gridHasAWinner $ grid horizontalTestGrid

diagonalTestGrid =
  let grid = Seq.fromList $ map Seq.fromList cols
      e = emptyCell
      w = Cell $ Just $ Disc White
      b = Cell $ Just $ Disc Black
      cols = [[e, e, e, e, e, e, w]
             ,[e, e, e, e, e, w, b]
             ,[e, e, e, e, w, b, b]
             ,[e, e, e, w, b, w, w]
             ,[e, e, e, e, e, e, b]
             ,[e, e, e, e, e, e, e]
             ,[e, e, e, e, e, e, e]
             ,[e, e, e, e, e, e, e]
             ]
  in GameGrid (8, 7) grid

reverseDiagonalTestGrid =
  let grid = Seq.fromList $ map Seq.fromList cols
      e = emptyCell
      w = Cell $ Just $ Disc White
      b = Cell $ Just $ Disc Black
      cols = [[e, e, e, b, b, b, w]
             ,[e, e, e, e, b, w, b]
             ,[e, e, e, e, w, b, b]
             ,[e, e, e, b, b, w, b]
             ,[e, e, e, e, e, e, e]
             ,[e, e, e, e, e, e, e]
             ,[e, e, e, e, e, e, e]
             ,[e, e, e, e, e, e, e]
             ]
  in GameGrid (8, 7) grid

verticalTestGrid =
  let grid = Seq.fromList $ map Seq.fromList cols
      e = emptyCell
      w = Cell $ Just $ Disc White
      b = Cell $ Just $ Disc Black
      cols = [[e, e, e, b, b, b, b]
             ,[e, e, e, e, b, w, b]
             ,[e, e, e, e, w, b, b]
             ,[e, e, e, b, b, w, w]
             ,[e, e, e, e, e, e, e]
             ,[e, e, e, e, e, e, e]
             ,[e, e, e, e, e, e, e]
             ,[e, e, e, e, e, e, e]
             ]
  in GameGrid (8, 7) grid

horizontalTestGrid =
  let grid = Seq.fromList $ map Seq.fromList cols
      e = emptyCell
      w = Cell $ Just $ Disc White
      b = Cell $ Just $ Disc Black
      cols = [[e, e, e, b, b, b, w]
             ,[e, e, e, e, b, w, b]
             ,[e, e, e, e, w, b, b]
             ,[e, e, e, b, b, w, b]
             ,[e, e, e, e, e, e, b]
             ,[e, e, e, e, e, e, e]
             ,[e, e, e, e, e, e, e]
             ,[e, e, e, e, e, e, e]
             ]
  in GameGrid (8, 7) grid

