module Controls where

import Pacman   as Pac
import Array (Array)
import Array    as Arr
import List     as Lst
import Keyboard as Key

wallLocs : Array (Array Pac.Box)
wallLocs =
  Arr.fromList <| Lst.map Arr.fromList Pac.initBoard

isWall : Pac.Pos -> Bool
isWall (x, y) =
  let
    cx = clamp 0 Pac.numCols <| round x
    cy = clamp 0 Pac.numRows <| round y
    (Just rw) = Arr.get cy wallLocs
    (Just bx) = Arr.get cx rw
  in
    case bx of
      Pac.Wall -> True
      _    -> False

pos_add : Pac.Pos -> Pac.Pos -> Pac.Pos
pos_add (x, y) (x', y') =
  (x + x', y + y')

updatePacPos : Pac.Pacman -> Pac.Pacman
updatePacPos pacman_old =
  let
    old_pos = pacman_old.pos
    d       = pacman_old.dir
    delta   = 0.5
    new_pos =
      case d of
        Pac.Left  -> pos_add old_pos (-delta,  0.0)
        Pac.Right -> pos_add old_pos ( delta,  0.0)
        Pac.Up    -> pos_add old_pos ( 0.0, -delta)
        Pac.Down  -> pos_add old_pos ( 0.0,  delta)
  in
    {pacman_old | pos <- if isWall new_pos
                         then old_pos
                         else new_pos}

--should be used to update dir when most recent key pressed is arrow key
updateDir : Key.KeyCode -> Pac.Pacman -> Pac.Pacman
updateDir last_key pacman_old =
  let
    dir_old = pacman_old.dir
    dir_new =
      if | last_key == 37 -> Pac.Left
         | last_key == 38 -> Pac.Up
         | last_key == 39 -> Pac.Right
         | last_key == 40 -> Pac.Down
         | otherwise      -> dir_old
  in
    {pacman_old | dir <- dir_new}
