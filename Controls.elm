module Controls where

import Pacman   as Pac
import Array exposing (Array)
import Array    as Arr
import List     as Lst
import Keyboard as Key

wallLocs : Array (Array Pac.Box)
wallLocs =
  Arr.fromList <| Lst.map Arr.fromList Pac.initBoard

isBarrier : Pac.Pos -> Bool -> Bool
isBarrier (x, y) careAboutGates =
    let
      isB = \b -> b == Pac.Wall || (b == Pac.Gate && careAboutGates)
      (cxl, cxr) = (clamp 0 (Pac.numCols - 1) <| floor   x,
                          clamp 0 (Pac.numCols - 1) <| ceiling x)
      (cyu, cyd) = (clamp 0 (Pac.numRows - 1) <| floor   y,
                          clamp 0 (Pac.numRows - 1) <| ceiling y)
      (Just rwu,  Just rwd) = (Arr.get cyu wallLocs, Arr.get cyd wallLocs)
      (Just bxul, Just bxur, Just bxdl, Just bxdr) =
        (Arr.get cxl rwu, Arr.get cxl rwd, Arr.get cxr rwu, Arr.get cxr rwd)
    in
      case (isB bxul, isB bxur, isB bxdl, isB bxdr) of
        (True, _, _, _) -> True
        (_, True, _, _) -> True
        (_, _, True, _) -> True
        (_, _, _, True) -> True
        _               -> False

pos_add : Pac.Pos -> Pac.Pos -> Pac.Pos
pos_add (x, y) (x', y') =
  (x + x', y + y')

snapToWall : Pac.Pos -> Pac.Dir -> Pac.Pos
snapToWall (x, y) d =
  case d of
    Pac.Left  -> (toFloat <| floor x, y)
    Pac.Right -> (toFloat <| ceiling x, y)
    Pac.Up    -> (x, toFloat <| floor y)
    Pac.Down  -> (x, toFloat <| ceiling y)

updatePos : Pac.Pos -> Pac.Dir -> Float -> Pac.Pos
updatePos pos d delta =
  case d of
    Pac.Left  -> pos_add pos (-delta,  0.0)
    Pac.Right -> pos_add pos ( delta,  0.0)
    Pac.Up    -> pos_add pos ( 0.0, -delta)
    Pac.Down  -> pos_add pos ( 0.0,  delta)

updatePacPos : Pac.Pacman -> Bool -> Pac.Pacman
updatePacPos pacman_old ghosts_frightened =
  let
    old_pos = pacman_old.pos
    d       = pacman_old.dir
    delta   = 0.5 * pacman_old.difMulti 
                  * (if   ghosts_frightened
                     then pacman_old.spdMultis.scrd
                     else pacman_old.spdMultis.norm)
    new_pos = updatePos old_pos d delta
    (newx, newy) = new_pos
  in
    {pacman_old | pos <-
                    if | isBarrier new_pos True         -> snapToWall pacman_old.pos pacman_old.dir
                       | newx > toFloat Pac.numCols - 1 -> (0, newy)
                       | newx < 0                       -> (toFloat Pac.numCols - 1, newy)
                       | otherwise                      -> new_pos
                , prvPos <-
                    if | newx > toFloat Pac.numCols - 1 -> (-1, newy)
                       | newx < 0                       -> (toFloat Pac.numCols, newy)
                       | otherwise                      -> pacman_old.pos}

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
