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

-- isPellet : Pac.Pos -> Pac.Board -> Bool
removePellet : Pac.Pos -> Pac.Board -> (Bool, Array (Array Pac.Box))
removePellet (x, y) bd =
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
      (Pac.Pellet, _, _, _) -> (True, modifyRow cxl cyu bd')
      (_, Pac.Pellet, _, _) -> (True, modifyRow cxl cyd bd')
      (_, _, Pac.Pellet, _) -> (True, modifyRow cxr cyu bd')
      (_, _, _, Pac.Pellet) -> (True, modifyRow cxr cyd bd')
      _                   -> (False, bd')

updateBoard : Pac.Board -> Pac.Pacman -> Pac.Board
updateBoard board_old pacman_old =
    let (b, bd) =  (removePellet pacman_old.pos board_old) in
    if b then atob bd else board_old