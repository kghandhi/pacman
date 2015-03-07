module GhostControls where

import Controls (..)
import Pacman as Pac
import List   as Lst

dist (x1, y1) (x2, y2) = sqrt ((x2-x1)^2 + (y2-y1)^2)

--moveToTarget (x, y) g

--updatePinkyPos : Pac.Pacman -> Pac.Ghost -> Pac.Pacman
--updatePinkyPos p pinky_old =
--    let
--        old_pos = pinky_old.pos
--        delta = 0.5
--        (px, py) = p.pos
--        chaseTarget = case p.dir of
--                        Pac.Left -> (max (px - 4) 0, py)
--                        Pac.Right -> (min (px + 4) 27, py)
--                        Pac.Up -> (px, max (py - 4) 0)
--                        Pac.Down -> (px, min (py + 4) 30)
--    in
--      case pinky_old.mode of
--
--        Chase ->
--        -- move towards the target. If you have the oportunity to change directions do it
--        Scatter ->
--        Flee ->

updateScatterMode : Pac.Ghost -> Pac.Ghost
updateScatterMode g =
  let
    delta                  = 0.25
    (gx, gy)               = g.pos
    curDirOrBarrier d      =
      case d of
        Pac.Left  -> g.dir /= Pac.Right && (not <| isBarrier (gx - delta, gy        ))
        Pac.Right -> g.dir /= Pac.Left  && (not <| isBarrier (gx + delta, gy        ))
        Pac.Up    -> g.dir /= Pac.Down  && (not <| isBarrier (gx        , gy - delta))
        Pac.Down  -> g.dir /= Pac.Up    && (not <| isBarrier (gx        , gy + delta))
    legal_dirs             = Lst.filter curDirOrBarrier [Pac.Left, Pac.Right, Pac.Up, Pac.Down]
    distToTarg pos         =
      dist g.target pos 
    dirs_and_dists         = Lst.map 
                               (\dr -> 
                                  let
                                    n_pos = updatePos g.pos dr delta
                                  in
                                    (dr, n_pos, distToTarg n_pos))
                               legal_dirs
    max_dst (dr1, ps1, dst1) (dr2, ps2, dst2) =
      if | dst1 < dst2 -> (dr1, ps1, dst1)
         | otherwise   -> (dr2, ps2, dst2)
    (new_dir, new_pos, _) = Lst.foldl1 max_dst dirs_and_dists
  in
    {g | dir <- new_dir, pos <- new_pos}


--isScared atePill g =
--    if atePill then {g | self <- Scared}