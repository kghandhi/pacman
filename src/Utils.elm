module Utils where

import Pacman as Pac

--linear interpolation
lerp : Float -> Float -> Float -> Float -> Float -> Float
lerp omin omax imin xx imax =
  let
    delta_o = omax - omin
    delta_i = imax - imin
    x_min_i = xx - imin
  in
    omin + (delta_o * x_min_i / delta_i)

--converts from array index space to collage world space
itow : Int -> Int -> (Pac.Pos) -> (Pac.Pos)
itow w h (x, y) =
  let
    (fw, fh) = (toFloat w, toFloat h)
    (fcols, frows) = (toFloat Pac.numCols, toFloat Pac.numRows)
  in
    ((fw / (2 * fcols)) + (lerp (-fw / 2) (fw / 2) 0 x fcols),
     (-fh / (2 * frows)) + (lerp (-fh / 2) (-50 + fh / 2) frows y 0))
    -- -50 is to account for height of the title

--converts from collage world space to array index space
wtoi : Int -> Int -> (Pac.Pos) -> (Pac.Pos)
wtoi w h (x, y) =
  let
    (fw, fh) = (toFloat w, toFloat h)
    (fcols, frows) = (toFloat Pac.numCols, toFloat Pac.numRows)
  in
    (-0.5 + (lerp 0 (fcols - 1) (-fw / 2) x (fw / 2)),
     -0.5 + (lerp (frows - 1) 0 (-fh / 2) y (fh / 2)))
