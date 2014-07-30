module Main where

import ConnectFour

getMove :: Color -> IO Int
getMove color = do
  putStrLn $ (show color) ++ ":"
  i <- getLine
  return $ (read i) - 1

playGame :: Board -> Color -> IO ()
playGame board color = do
  printBoard board
  i <- getMove color
  let isValidMove = validMove board i
  if isValidMove then do
    let newBoard = playerMove board color i
    printBoard newBoard
    let win = playerHasWon newBoard color
    if win then do
      putStrLn $ "Player " ++ (show color) ++ " wins!"
    else do
      playGame newBoard (getOtherColor color)
  else do
    putStrLn $ "Invalid Move"
    playGame board color

main = do
  putStrLn "Welcome to ConnectFourHS"
  boardSize <- getBoardSize
  let board = setupBoard (fst boardSize) (snd boardSize)
  playGame board Red
