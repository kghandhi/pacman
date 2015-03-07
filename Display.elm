module Display where

import Pacman (..)
import Controls as Ctr
import BoardControls as BCtr
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
font = ["Andale Mono", "monospace"]

title w h =
    El.container w (h + 20) El.middle
          <| El.flow El.down
                 [El.image w h "/pacman-logo.jpg", El.spacer w 20]

renderPacman : Pacman -> Int -> El.Element
renderPacman p bSide =
    let
        pman_form =
            case p.dir of
              Left -> Mod.pacman Mod.Left ((toFloat bSide) / 2)
              Right -> Mod.pacman Mod.Right ((toFloat bSide) / 2)
              Up -> Mod.pacman Mod.Up ((toFloat bSide) / 2)
              Down -> Mod.pacman Mod.Down ((toFloat bSide) / 2)
    in
      Clg.collage bSide bSide [pman_form]

displayBox : Box -> Int -> El.Element
displayBox b bSide =
    case b of
      Empty -> Clg.collage bSide bSide [Mod.emptySpace (toFloat bSide)]
      Pellet -> Clg.collage bSide bSide [Mod.pellet ((toFloat bSide) / 6)]
      Pill -> Clg.collage bSide bSide [Mod.pill ((toFloat bSide) / 3)]
      Wall -> Clg.collage bSide bSide [Mod.wall (toFloat bSide)]
      Gate -> Clg.collage bSide bSide [Mod.gate (toFloat bSide)]

displayLives : Int -> Int -> El.Element
displayLives lives sz =
    let
        life = Clg.collage sz sz [Mod.pacman Mod.Right ((toFloat sz) / 2)]
    in
      El.flow El.left (List.map (\_ -> life) [1..lives])

magicNumber = 45

scoreStyle : Txt.Style
scoreStyle = {typeface = font
             , height = Just magicNumber
             , color = white
             , bold = False
             , italic = False
             , line = Nothing
             }

renderGhost g bSide =
    let
        h = toFloat bSide
        w = h
    in
      if g.self == Scared then Mod.ghost "scared" w h
      else Mod.ghost g.name w h

view : (Int, Int) -> State -> El.Element
view (w, h) st =
    let
        bSide = (h - magicNumber) // 36
        titleHeight = 30
        titleWidth = bSide * 23

        ttl = title titleWidth titleHeight

        score = "SCORE: " ++ (toString st.points)
              |> Txt.fromString
              |> Txt.style scoreStyle
              |> Txt.leftAligned
        scoreLives = El.flow El.right [score, (displayLives st.extraLives magicNumber)]

        rowBuilder bxs = El.flow El.left (List.map (\b -> displayBox b bSide) bxs)
        colBuilder rws = El.flow El.down ([scoreLives] ++ rws)

        pinky_pos = Utl.itow (bSide * numCols) (titleHeight + 20 + (bSide * numRows)) st.pinky.pos
        inky_pos = Utl.itow (bSide * numCols) (titleHeight + 20 + (bSide * numRows)) st.inky.pos
        blinky_pos = Utl.itow (bSide * numCols) (titleHeight + 20 + (bSide * numRows)) st.blinky.pos
        clyde_pos = Utl.itow (bSide * numCols) (titleHeight + 20 + (bSide * numRows)) st.clyde.pos
        pac_pos = Utl.itow (bSide * numCols) (titleHeight + 20 + (bSide * numRows)) st.pacman.pos
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
                 , Clg.move pinky_pos <| renderGhost st.pinky bSide
                 , Clg.move inky_pos <| renderGhost st.inky bSide
                 , Clg.move blinky_pos <| renderGhost st.blinky bSide
                 , Clg.move clyde_pos <| renderGhost st.clyde bSide
                 ]

--Controller

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
    TimeAction  ->
        let
            (extra_pts, newBoard) = BCtr.updateBoard s.board s.pacman
            old_pts = s.points
            atePill = extra_pts == pillPoint
            atePell = extra_pts == pelletPoint
            old_pellsAte = s.pellsAte
        in
        {s | pacman <- Ctr.updatePacPos s.pacman
        , board <- newBoard
        , points <- old_pts + extra_pts
        , pellsAte <- if atePell then old_pellsAte + 1 else old_pellsAte}
-- if extra_pts == 50 -> Pill, then update the ghosts.

main : Signal El.Element
main = view <~ Window.dimensions ~ currState
