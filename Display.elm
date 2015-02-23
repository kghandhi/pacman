module Display where

import Pacman (..)
import Controls as Ctr
import Models   as Mod
import Utils    as Utl
import Time (..)

import List ((::))
import List
import Array as A

import Color (..)
import Signal (Signal, (<~), (~))
import Signal
import Window
import Mouse
import Text as Txt
import Graphics.Element as El
import Graphics.Collage as Clg
import String
import Keyboard as Key

-- View
testerFun : Box -> Int -> El.Element
testerFun b bSide =
    case b of
      Empty -> Clg.collage bSide bSide [Mod.emptySpace (toFloat bSide)]
      Pellet -> Clg.collage bSide bSide [Mod.pellet ((toFloat bSide) / 6)]
      Pill -> Clg.collage bSide bSide [Mod.pill ((toFloat bSide) / 3)]
      Wall -> Clg.collage bSide bSide [Mod.wall (toFloat bSide)]


view : (Int, Int) -> State -> El.Element
view (w, h) st =
    let
        bSide = h // 36
        rowBuilder bxs = El.flow El.left (List.map (\b -> testerFun b bSide) bxs)
        colBuilder rws = El.flow El.down rws
        pac_pos = Utl.itow (bSide * numCols) (bSide * numRows) st.pacman.pos
        pac_dir = case st.pacman.dir of
                    Left -> Mod.Left
                    Right -> Mod.Right
                    Up -> Mod.Up
                    Down -> Mod.Down
    in
      El.color black
            <| Clg.collage w h
                 [ Clg.toForm <| colBuilder (List.map rowBuilder st.board)
                 , Clg.move pac_pos <| Mod.pacman pac_dir <| toFloat <| bSide // 2
                 ]

-- Controller

type Action = KeyAction Key.KeyCode | TimeAction

actions : Signal Action
actions =
  Signal.merge
    (Signal.map (\k -> KeyAction k) Key.lastPressed)
    (Signal.sampleOn (every <| second / 40)  <| Signal.constant TimeAction)

currState : Signal State
currState =
  Signal.dropRepeats
    <| Signal.foldp upstate initState actions

upstate : Action -> State -> State
upstate a s =
  case a of
    KeyAction k -> {s | pacman <- Ctr.updateDir  k s.pacman}
    TimeAction  -> {s | pacman <- Ctr.updatePacPos s.pacman}

main : Signal El.Element
main = view <~ Window.dimensions ~ currState