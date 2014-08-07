module Main where

import ConnectFour

getMove :: IO Int
getMove = do i <- getLine
             return $ read i

playGame :: Board -> Color -> IO ()
playGame board color = do
  putStrLn $ (show color) ++ " Move:"
  move <- getMove
  if validInsert board color move then do
    let (newBoard, coords) = insertColor board color move
    let newGameState = evaluateBoard newBoard color coords
    putStrLn (show newBoard)
    case newGameState of Draw -> putStrLn "Draw!"
                         Win color -> putStrLn $ show newGameState
                         NextMove newColor -> playGame newBoard newColor
  else do
    putStrLn "Invalid Move!"
    playGame board color

main = do
  putStrLn "Welcome to ConnectFourHS"
  let board = initializeBoard
  putStrLn $ show board
  playGame board Red
