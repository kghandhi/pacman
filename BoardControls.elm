module BoardControls where

import Pacman as Pac
import Array (Array)
import Array as Arr
import List

btoa : Pac.Board -> Array (Array Pac.Box)
btoa bd = Arr.fromList <| List.map (Arr.fromList << List.reverse) bd

atob : Array (Array Pac.Box) -> Pac.Board
atob bd =  Arr.toList <| Arr.map (List.reverse << Arr.toList) bd

-- modifyRow: Int -> Array Pac.Box -> Array (Array Pac.Box) -> Array (Array Pac.Box)
modifyRow cl_ix rw_ix bd =
    let
        (Just rw) = Arr.get rw_ix bd
        updatedRow = Arr.set cl_ix Pac.Empty rw
    in
      Arr.set rw_ix updatedRow bd

removeTreat : Pac.Pos -> Pac.Box -> Pac.Board -> (Bool, Array (Array Pac.Box))
removeTreat (x, y) treat bd =
    let
      bd' = btoa bd
      (cxl, cxr) = (clamp 0 (Pac.numCols - 1) <| floor  x,
                          clamp 0 (Pac.numCols - 1) <| ceiling x)
      (cyu, cyd) = (clamp 0 (Pac.numRows - 1) <| floor   y,
                    clamp 0 (Pac.numRows - 1) <| ceiling y)
      (Just rwu,  Just rwd) = (Arr.get cyu bd', Arr.get cyd bd')
      (Just bxul, Just bxur, Just bxdl, Just bxdr) =
          (Arr.get cxl rwu, Arr.get cxl rwd, Arr.get cxr rwu, Arr.get cxr rwd)
  in
    case (bxul, bxur, bxdl, bxdr) of
      (treat, _, _, _) -> (True, modifyRow cxl cyu bd')
      (_, treat, _, _) -> (True, modifyRow cxl cyd bd')
      (_, _, treat, _) -> (True, modifyRow cxr cyu bd')
      (_, _, _, treat) -> (True, modifyRow cxr cyd bd')
      _                   -> (False, bd')

updateBoard : Pac.Board -> Pac.Pacman -> (Int, Pac.Board)
updateBoard board_old pacman_old =
    let
        (bPells, board_new1) =  removeTreat pacman_old.pos Pac.Pellet board_old
        (bPills, board_new2) = removeTreat pacman_old.pos Pac.Pill board_old
    in
      if | bPells -> (Pac.pelletPoint, atob board_new1)
         | bPills -> (Pac.pillPoint, atob board_new2)
         | otherwise -> (0, board_old)