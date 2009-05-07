--- Copyright © 2009 Bart Massey
--- ALL RIGHTS RESERVED
--- [This program is licensed under the "MIT License"]
--- Please see the file COPYING in the source
--- distribution of this software for license terms.

module Board (
  Coord, Move, Board, Position,
  snapshotBoard, animatePosition,
  startPosition,
  getBoardSquare, setBoardSquare,
  getPositionSquare,
  showMove, readMove, movePiece,
  showPosition, showBoard, readPosition, hGetPosition,
  squares,
  dirnUp, dirnRight, dirnDown, dirnLeft,
  dirnUL, dirnUR, dirnDR, dirnDL,
  evalBoardForWhite
) where

import Data.List
import Data.Int
import Data.Char
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import System.IO

type Coord = (Char, Int8)
type Move = (Coord, Coord)
newtype Board s = Board (STUArray s Coord Char)
newtype Position = Position (UArray Coord Char)

snapshotBoard :: Board s -> ST s Position
snapshotBoard (Board b) = do
  b' <- freeze b
  return (Position b')

animatePosition :: Position -> ST s (Board s)
animatePosition (Position b) = do
  b' <- thaw b
  return (Board b')

startPosition :: Position
startPosition =
  Position (listArray (('a', 1), ('e', 6)) [
    'R', 'P', '.', '.', 'p', 'k',
    'N', 'P', '.', '.', 'p', 'q',
    'B', 'P', '.', '.', 'p', 'b',
    'Q', 'P', '.', '.', 'p', 'n',
    'K', 'P', '.', '.', 'p', 'r' ])

getBoardSquare :: Board s -> Coord -> ST s Char
getBoardSquare (Board b) coord = readArray b coord

setBoardSquare :: Board s -> Coord -> Char -> ST s ()
setBoardSquare (Board b) coord piece = writeArray b coord piece

getPositionSquare :: Position -> Coord -> Char
getPositionSquare (Position b) coord = b ! coord

row_char :: Int8 -> Char
row_char r = chr ((ord '0') + (fromIntegral r))

showMove :: Move -> String
showMove ((fc, fr), (tc, tr)) = [fc, row_char fr, '-', tc, row_char tr]

row_number :: Char -> Int8
row_number r = fromIntegral ((ord r) - (ord '0'))

readMove :: String -> Maybe Move
readMove [fc, fr, '-', tc, tr] =
    if ok_col fc && ok_col tc &&
       ok_row fr && ok_row tr
    then Just ((fc, row_number fr), (tc, row_number tr))
    else Nothing
    where
      ok_col c = c >= 'a' && c <= 'e'
      ok_row r = r >= '1' && r <= '6'
readMove _ = Nothing

movePiece :: Board s -> Move -> ST s (Char, Bool)
movePiece board (start, end@(_, r)) = do
  moving_piece <- getBoardSquare board start
  when (moving_piece == '.')
       (error "internal error: moving empty space")
  old_piece <- getBoardSquare board end
  setBoardSquare board start '.'
  setBoardSquare board end moving_piece
  promoted <- promote moving_piece r
  return (old_piece, promoted)
  where
    promote 'P' 6 = do
      setBoardSquare board end 'Q'
      return True
    promote 'p' 1 = do
      setBoardSquare board end 'q'
      return True
    promote _ _ = return False

showPosition :: Position -> String
showPosition position =
  let square_list = map make_row [1 .. 6]
  in
    unlines (reverse square_list)
  where
    make_row r = map (\c -> getPositionSquare position (c, r)) ['a' .. 'e']

showBoard :: Board s -> ST s String
showBoard brd = do
  posn <- snapshotBoard brd
  return (showPosition posn)

readPosition :: String -> Position
readPosition pstring =
    let rows = lines pstring
        els = concat (transpose (reverse rows))
        b = listArray (('a', 1), ('e', 6)) els
    in
      Position b

hGetPosition :: Handle -> IO Position
hGetPosition h = do
  rows <- replicateM 6 (hGetLine h)
  return (readPosition (unlines rows))

squares :: [Coord]
squares = [ (c, r) | c <- ['a' .. 'e'], r <- [1 .. 6] ]

dirnUp :: Coord -> Coord
dirnUp (c, r) = (c, r + 1)

dirnDown :: Coord -> Coord
dirnDown (c, r) = (c, r - 1)

dirnLeft :: Coord -> Coord
dirnLeft (c, r) = (pred c, r)

dirnRight :: Coord -> Coord
dirnRight (c, r) = (succ c, r)

dirnUL :: Coord -> Coord
dirnUL (c, r) = (pred c, r + 1)

dirnUR :: Coord -> Coord
dirnUR (c, r) = (succ c, r + 1)

dirnDL :: Coord -> Coord
dirnDL (c, r) = (pred c, r - 1)

dirnDR :: Coord -> Coord
dirnDR (c, r) = (succ c, r - 1)

evalBoardForWhite :: Board s -> ST s Int
evalBoardForWhite (Board brd) = do
    es <- getElems brd
    return (foldl (\v c -> v + sqval c) 0 es)
    where
      sqval '.' = 0
      sqval 'K' = 50000
      sqval 'k' = -50000
      sqval 'P' = 10
      sqval 'p' = -10
      sqval 'Q' = 90
      sqval 'q' = -90
      sqval 'R' = 50
      sqval 'r' = -50
      sqval 'B' = 20
      sqval 'b' = -20
      sqval 'N' = 40
      sqval 'n' = -40
      sqval _ = error "internal error: bad square value"
