module GhostControls where

import Controls (..)
import Pacman as Pac

dist (x1, y1) (x2, y2) = sqrt ((x2-x1)^2 + (y2-y1)^2)

moveToTarget (x, y) g

updatePinkyPos : Pac.Pacman -> Pac.Ghost -> Pac.Pacman
updatePinkyPos p pinky_old =
    let
        old_pos = pinky_old.pos
        delta = 0.5
        (px, py) = p.pos
        chaseTarget = case p.dir of
                        Pac.Left -> (max (px - 4) 0, py)
                        Pac.Right -> (min (px + 4) 27, py)
                        Pac.Up -> (px, max (py - 4) 0)
                        Pac.Down -> (px, min (py + 4) 30)
    in
      case pinky_old.mode of

        Chase ->
        -- move towards the target. If you have the oportunity to change directions do it
        Scatter ->
        Flee ->



isScared atePill g =
    if atePill then {g | self <- Scared}