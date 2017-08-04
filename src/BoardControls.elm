module BoardControls where

import Pacman as Pac
import Array exposing (Array)
import Array as Arr
import List
import Maybe exposing (withDefault)

btoa : Pac.Board -> Array (Array Pac.Box)
btoa bd = Arr.fromList <| List.map (Arr.fromList << List.reverse) bd

atob : Array (Array Pac.Box) -> Pac.Board
atob bd =  Arr.toList <| Arr.map (List.reverse << Arr.toList) bd

modifyRow: Int -> Int -> Array (Array Pac.Box) -> Array (Array Pac.Box)
modifyRow cl_ix rw_ix bd =
  let
    rw = withDefault Array.empty (Arr.get rw_ix bd)
    updatedRow = Arr.set cl_ix Pac.Empty rw
  in
    Arr.set rw_ix updatedRow bd

removeTreat : Pac.Pos -> (Pac.Box -> Bool) -> Pac.Board -> (Bool, Array (Array Pac.Box))
removeTreat (x, y) treat_fn bd =
  let
    bd' = btoa bd
    (cxl, cxr) = (clamp 0 (Pac.numCols - 1) <| floor  x,
                  clamp 0 (Pac.numCols - 1) <| ceiling x)
    (cyu, cyd) = (clamp 0 (Pac.numRows - 1) <| floor   y,
                  clamp 0 (Pac.numRows - 1) <| ceiling y)
    rwu  = withDefault Array.empty (Arr.get cyu bd')
    rwd  = withDefault Array.empty (Arr.get cyd bd')
    bxul = withDefault Pac.Empty (Arr.get cxl rwu)
    bxur = withDefault Pac.Empty (Arr.get cxl rwd)
    bxdl = withDefault Pac.Empty (Arr.get cxr rwu)
    bxdr = withDefault Pac.Empty (Arr.get cxr rwd)
  in
    if treat_fn bxul then
      (True, modifyRow cxl cyu bd')
    else if treat_fn bxur then
      (True, modifyRow cxl cyd bd')
    else if treat_fn bxdl then
      (True, modifyRow cxr cyu bd')
    else if treat_fn bxdr then
      (True, modifyRow cxr cyd bd')
    else
      (False, bd')

isPellet : Pac.Box -> Bool
isPellet t =
    case t of
      Pac.Pellet -> True
      _ -> False

isPill : Pac.Box -> Bool
isPill t =
    case t of
      Pac.Pill -> True
      _ -> False

updateBoard : Pac.Board -> Pac.Pacman -> (Int, Pac.Board)
updateBoard board_old pacman_old =
    let
        (bPells, board_new1) = removeTreat pacman_old.pos isPellet board_old
        (bPills, board_new2) = removeTreat pacman_old.pos isPill board_old
    in
      if bPells then
        (Pac.pelletPoint, atob board_new1)
      else if bPills then
        (Pac.pillPoint, atob board_new2)
      else
        (0, board_old)
