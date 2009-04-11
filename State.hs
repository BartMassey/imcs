--- Copyright © 2009 Bart Massey
--- ALL RIGHTS RESERVED
--- [This program is licensed under the "MIT License"]
--- Please see the file COPYING in the source
--- distribution of this software for license terms.

module State (
  Side(..), State(..), Problem(..), Undo(..),
  opponent,
  readSide, readProblem, showProblem, showSide,
  startProblem, moves, problemMoves,
  move, unmove, gameOver,
  snapshotState, animateProblem,
  eval
) where

import Data.Char
import Control.Monad
import Control.Monad.ST

import Board

data Side = White | Black | None deriving Eq

opponent :: Side -> Side
opponent White = Black
opponent Black = White

data State s = State { toMove :: Side,
                       turn :: Int,
                       board :: Board s }
                    
data Problem = Problem { problemToMove :: Side,
                         problemTurn :: Int,
                         position :: Position }
                    
showSide :: Side -> Char
showSide White = 'W'
showSide Black = 'B'
showSide _ = '?'

readProblem :: String -> Problem
readProblem desc =
    let (info : board) = lines desc
        [n, [side]] = words info in
    Problem { problemToMove = readSide side,
              problemTurn = read n,
              position = readPosition (unlines board) }

readSide :: Char -> Side
readSide 'W' = White
readSide 'B' = Black

showProblem :: Problem -> String
showProblem (Problem { problemToMove = side,
                       problemTurn = n,
                       position = posn }) =
   let bdesc = showPosition posn in
       show n ++ " " ++ [showSide side] ++ "\n" ++ bdesc

startProblem :: Problem
startProblem =  Problem { problemToMove = White,
                          problemTurn = 1,
                          position = startPosition }

piece_side :: Char -> Side
piece_side p | p >= 'A' && p <= 'Z' = White
piece_side p | p >= 'a' && p <= 'z' = Black
piece_side _ = None

find_pieces :: Board s -> Side -> ST s [(Coord, Char)]
find_pieces brd side = do
  candidates <- mapM candidate squares
  return (filter (\(_, p) -> piece_side p == side) candidates)
  where
    candidate s = do
      p <- getBoardSquare brd s
      return (s, p)

landable :: Board s -> Side -> Coord -> ST s Bool
landable brd side square = do
  p <- getBoardSquare brd square
  return (piece_side p /= side)


raytrace :: (Coord -> Coord)
      -> (Coord -> Bool)
      -> Board s
      -> Side
      -> Coord
      -> ST s [Coord]
raytrace rayfn clipfn brd side square =
    block (takeWhile clipfn (tail (iterate rayfn square)))
    where
      block [] = return []
      block (s : ss) = do
        p <- getBoardSquare brd s
        case piece_side p of
          None -> do
            rest <- block ss
            return (s : rest)
          side' -> if side' == side
                      then return []
                      else return [s]

--- XXX I'm too lazy to explicitly type these
trace_up = raytrace dirnUp (\(_, r) -> r <= 6)
trace_right = raytrace dirnRight (\(c, _) -> c <= 'e')      
trace_down = raytrace dirnDown (\(_, r) -> r >= 1)
trace_left = raytrace dirnLeft (\(c, _) -> c >= 'a')
trace_ul = raytrace dirnUL (\(c, r) -> c >= 'a' && r <= 6)
trace_ur = raytrace dirnUR (\(c, r) -> c <= 'e' && r <= 6)
trace_dr = raytrace dirnDR (\(c, r) -> c <= 'e' && r >= 1)
trace_dl = raytrace dirnDL (\(c, r) -> c >= 'a' && r >= 1)      

jump :: (Coord -> Coord)
     -> (Coord -> Bool)
     -> Board s
     -> Side
     -> Coord
     -> ST s [Coord]
jump jumpfn clipfn brd side square = do
  let square' = jumpfn square
  if (clipfn square')
     then do
           ok <- landable brd side square'
           if ok
              then return [square']
              else return []
     else return []

--- XXX I'm too lazy to explicitly type these
jump_up = jump dirnUp (\(_, r) -> r <= 6)
jump_right = jump dirnRight (\(c, _) -> c <= 'e')
jump_down = jump dirnDown (\(_, r) -> r >= 1)
jump_left = jump dirnLeft (\(c, _) -> c >= 'a')
jump_ul = jump dirnUL (\(c, r) -> c >= 'a' && r <= 6)
jump_ur = jump dirnUR (\(c, r) -> c <= 'e' && r <= 6)
jump_dr = jump dirnDR (\(c, r) -> c <= 'e' && r >= 1)
jump_dl = jump dirnDL (\(c, r) -> c >= 'a' && r >= 1)

jump_uul = jump (dirnUp . dirnUL) (\(c, r) -> c >= 'a' && r <= 6)
jump_lul = jump (dirnLeft . dirnUL) (\(c, r) -> c >= 'a' && r <= 6)
jump_uur = jump (dirnUp . dirnUR) (\(c, r) -> c <= 'e' && r <= 6)
jump_rur = jump (dirnRight . dirnUR) (\(c, r) -> c <= 'e' && r <= 6)
jump_ddr = jump (dirnDown . dirnDR) (\(c, r) -> c <= 'e' && r >= 1)
jump_rdr = jump (dirnRight . dirnDR) (\(c, r) -> c <= 'e' && r >= 1)
jump_ddl = jump (dirnDown . dirnDL) (\(c, r) -> c >= 'a' && r >= 1)      
jump_ldl = jump (dirnLeft . dirnDL) (\(c, r) -> c >= 'a' && r >= 1)      

find_moves :: Board s -> Side -> Coord -> Char -> ST s [Move]
find_moves brd side square 'k' = do
  ups <- jump_up brd side square
  lefts <- jump_left brd side square
  downs <- jump_down brd side square
  rights <- jump_right brd side square
  uls <- jump_ul brd side square
  urs <- jump_ur brd side square
  drs <- jump_dr brd side square
  dls <- jump_dl brd side square
  return (zip (repeat square) (concat [ups, lefts, downs, rights,
                                       uls, urs, drs, dls]))
find_moves brd side square@(c, r) 'p' = do
  when unpromoted (error "internal error: unpromoted pawn")
  left <- capturable ('a' <=) (pred c, next_rank)
  right <- capturable ('e' >=) (succ c, next_rank)
  center <- empty (c, next_rank)
  return (zip (repeat square) (concat [left, center, right]))
  where
    unpromoted = case side of
                   White -> r >= 6
                   Black -> r <= 1
                   _ -> error "internal error: unpromoted non-side"
    next_rank = case side of
                   White -> r + 1
                   Black -> r - 1
                   _ -> error "internal error: next_rank non-side"
    capturable pr s@(c', r') = do
      if pr c'
         then do
               p <- getBoardSquare brd s
               let side' = piece_side p
               if side' == None || side' == side
                  then return []
                  else return [s]
         else return []
    empty s = do
      p <- getBoardSquare brd s
      if p == '.'
         then return [s]
         else return []
find_moves brd side square 'r' = do
  ups <- trace_up brd side square
  lefts <- trace_left brd side square
  downs <- trace_down brd side square
  rights <- trace_right brd side square
  return (zip (repeat square) (concat [ups, lefts, downs, rights]))
find_moves brd side square 'b' = do
  uls <- trace_ul brd side square
  urs <- trace_ur brd side square
  drs <- trace_dr brd side square
  dls <- trace_dl brd side square
  return (zip (repeat square) (concat [uls, urs, drs, dls]))
find_moves brd side square 'q' = do
  ups <- trace_up brd side square
  lefts <- trace_left brd side square
  downs <- trace_down brd side square
  rights <- trace_right brd side square
  uls <- trace_ul brd side square
  urs <- trace_ur brd side square
  drs <- trace_dr brd side square
  dls <- trace_dl brd side square
  return (zip (repeat square) (concat [ups, lefts, downs, rights,
                                       uls, urs, drs, dls]))
find_moves brd side square 'n' = do
  uul <- jump_uul brd side square
  lul <- jump_lul brd side square
  uur <- jump_uur brd side square
  rur <- jump_rur brd side square
  ddr <- jump_ddr brd side square
  rdr <- jump_rdr brd side square
  ddl <- jump_ddl brd side square
  ldl <- jump_ldl brd side square
  return (zip (repeat square) (concat [uul, lul, uur, rur,
                                       ddr, rdr, ddl, ldl]))
find_moves _ _ _ _ = error "internal error: find_moves non-piece"

moves :: State s -> ST s [Move]
moves (State { toMove = side,
               board = brd }) = do
  sources <- find_pieces brd side
  move_lists <- mapM compute_moves sources
  return (concat move_lists)
  where
    compute_moves (s, p) = find_moves brd side s (toLower p)

problemMoves :: Problem -> [Move]
problemMoves problem =
    runST (do
             state <- animateProblem problem
             movs <- moves state
             return movs)

data Undo = Undo { capture :: Char,
                   promoted :: Bool }

move :: State s -> Move -> ST s (State s, Undo)
move (State { toMove = who,
              turn = n,
              board = brd }) mov = do
  (ch, promoted) <- movePiece brd mov
  return (State { toMove = opponent who,
                  turn = n + bump_turn who,
                  board = brd },
          Undo { capture = ch,
                 promoted = promoted })
  where
    bump_turn White = 0
    bump_turn Black = 1

unmove :: State s -> Move -> Undo -> ST s ()
unmove (State { board = brd }) (src, dst) undo = do
  ('.', False) <- movePiece brd (dst, src)
  when (promoted undo) unqueen
  setBoardSquare brd dst (capture undo)
  where
    unqueen = do
      ss <- getBoardSquare brd src
      case ss of
        'Q' -> setBoardSquare brd src 'P'
        'q' -> setBoardSquare brd src 'p'

gameOver :: State s -> Undo -> ST s Bool
gameOver s undo =
    return (toLower (capture undo) == 'k' || turn s > 40)

snapshotState :: State s -> ST s Problem
snapshotState (State { toMove = m, turn = n, board = b }) =
    do
      p <- snapshotBoard b
      return (Problem { problemToMove = m,
                        problemTurn = n,
                        position = p })

animateProblem :: Problem -> ST s (State s)
animateProblem (Problem { problemToMove = m,
                          problemTurn = n,
                          position = p }) =
    do
      b <- animatePosition p
      return (State {toMove = m, turn = n, board = b})

eval :: State s -> ST s Int
eval (State { turn = n }) | n > 40 = return 0
eval (State { toMove = White, board = b }) = evalBoardForWhite b
eval (State { toMove = Black, board = b }) = do
  v <- evalBoardForWhite b
  return (-v)
