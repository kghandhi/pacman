module Controls where

import Pacman as Pac
import Array exposing (Array(..))
import List
import Char exposing (KeyCode)
import Maybe exposing (withDefault)

wallLocs : Array (Array Pac.Box)
wallLocs =
  Array.fromList <| List.map Array.fromList Pac.initBoard

isBarrier : Pac.Pos -> Bool -> Bool
isBarrier (x, y) careAboutGates =
    let
      isB = \b -> b == Pac.Wall || (b == Pac.Gate && careAboutGates)
      (cxl, cxr) = (clamp 0 (Pac.numCols - 1) <| floor   x,
                          clamp 0 (Pac.numCols - 1) <| ceiling x)
      (cyu, cyd) = (clamp 0 (Pac.numRows - 1) <| floor   y,
                          clamp 0 (Pac.numRows - 1) <| ceiling y)
      rwu = withDefault Array.empty (Array.get cyu wallLocs)
      rwd = withDefault Array.empty (Array.get cyd wallLocs)
      bxul = withDefault Pac.Empty (Array.get cxl rwu)
      bxur = withDefault Pac.Empty (Array.get cxl rwd)
      bxdl = withDefault Pac.Empty (Array.get cxr rwu)
      bxdr = withDefault Pac.Empty (Array.get cxr rwd)
    in
      if isB bxul || isB bxur || isB bxdl || isB bxdr then True else False

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
    {pacman_old | pos =
                    if isBarrier new_pos True then
                      snapToWall pacman_old.pos pacman_old.dir
                    else if newx > toFloat Pac.numCols - 1 then
                      (0, newy)
                    else if newx < 0 then
                      (toFloat Pac.numCols - 1, newy)
                    else
                      new_pos
                , prvPos =
                    if newx > toFloat Pac.numCols - 1 then
                      (-1, newy)
                    else if newx < 0 then
                      (toFloat Pac.numCols, newy)
                    else
                      pacman_old.pos}

--should be used to update dir when most recent key pressed is arrow key
updateDir : KeyCode -> Pac.Pacman -> Pac.Pacman
updateDir last_key pacman_old =
  let
    dir_old = pacman_old.dir
    dir_new =
      case last_key of
        37 -> Pac.Left
        38 -> Pac.Up
        39 -> Pac.Right
        40 -> Pac.Down
        _  -> dir_old
  in
    {pacman_old | dir = dir_new}
