module GhostControls where

import Controls (..)
import Random as R
import Pacman (..)

dist (x1, y1) (x2, y2) = sqrt ((x2-x1)^2 + (y2-y1)^2)

pfi : (Int, Int) -> Pos
pfi (x, y) = (toFloat x, toFloat y)


updateGhosts : State -> State
updateGhosts st atePill =
    let
        b = st.blinky
        p = st.pinky
        i = st.inky
        c = st.clyde
    in
      st


blinkyTarget : Ghost -> Pacman -> (Pos, Ghost)
blinkyTarget g p =
    case g.mode of
      Scatter -> (g.target, g)
      Chase -> (p.pos, g)
      Flee ->
          let
              gen = R.pair (R.int 0 (numCols - 1)) (R.int 0 (numRows - 1))
              ((rx, ry), new_seed) = R.generate gen g.seed
          in
            (pfi (rx,ry), {g | seed <- new_seed})

pinkyTarget : Ghost -> Pacman -> (Pos, Ghost)
pinkyTarget g p =
    case g.mode of
      Scatter -> (g.target, g)
      Chase -> let (px, py) = p.pos in
               case p.dir of
                 Left -> ((px-4, py), g)
                 Right -> ((px+4, py), g)
                 Down -> ((px, py+4), g)
                 Up -> ((px, py-4), g)
      Flee ->
          let
              gen = R.pair (R.int 0 (numCols - 1)) (R.int 0 (numRows - 1))
              ((rx, ry), new_seed) = R.generate gen g.seed
          in
            (pfi (rx,ry), {g | seed <- new_seed})

inkyTarget : Ghost -> Ghost -> Pacman -> (Pos, Ghost)
inkyTarget i b p =
    case i.mode of
      Scatter -> (i.target, i)
      Chase ->
          let
              (px, py) = p.pos
              (x, y) = case p.dir of
                         Left -> (px-2, py)
                         Right -> (px+2, py)
                         Down -> (px, py+2)
                         Up -> (px, py-2)
              (bx, by) = b.pos
          in
            ((2 * (x-bx), 2 * (y-by)), i)
      Flee ->
          let
              gen = R.pair (R.int 0 (numCols - 1)) (R.int 0 (numRows - 1))
              ((rx, ry), new_seed) = R.generate gen i.seed
          in
            (pfi (rx,ry), {i | seed <- new_seed})

clydeTarget : Ghost -> Pacman -> (Pos, Ghost)
clydeTarget g p =
    case g.mode of
      Scatter -> (g.target, g)
      Chase -> if (dist g.pos p.pos) < 8 then (p.pos, g)
               else (g.target, g)
      Flee ->
          let
              gen = R.pair (R.int 0 (numCols - 1)) (R.int 0 (numRows - 1))
              ((rx, ry), new_seed) = R.generate gen g.seed
          in
            (pfi (rx, ry), {g | seed <- new_seed})
