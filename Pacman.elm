module Pacman where

import Models as M
import List ((::))
import List ((++))
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

type alias Pos = (Float, Float)

-- Model

type alias State =
    { points : Int,
      extraLives : Int,
      gameState : GameState,
      board : Board,
      pacman : Pacman,
      blinky : Ghost,
      inky : Ghost,
      pinky : Ghost,
      clyde : Ghost
    }

type Dir = Left | Right | Up | Down

type alias Pacman =
    { pos : Pos,
      dir : Dir
    }

-- Chase: kill pacman, Flee: pacman can eat me, Scatter : circle/idle
type Mode = Chase | Flee | Scatter
-- Flee -> Scared. Dead -> Eye appearance
type Self = Normal | Scared | Dead

type alias Ghost =
    { pos : Pos,
      mode : Mode,
      target : Pos,
      self : Self
    }

type Box = Wall | Pellet | Empty | Fruit | Pill
type alias Row = List Box
type alias Board = List Row

type GameState = Active | Loading | Over

-- Boards are always w=28, h=31
numRows = 31
numCols = 28

allWalls : Int -> List Box
allWalls n = List.repeat n Wall

allPellets : Int -> List Box
allPellets n = List.repeat n Pellet

allEmpty : Int -> List Box
allEmpty n = List.repeat n Empty

-- essentially hardcoaded for the dimensions above. Woops.
initBoard : Board
initBoard =
    let
        r1 = allWalls (numCols // 2)
        r2 = List.concat [[Wall], allPellets ((numCols // 2) - 2), [Wall]]
        r3 = List.concat [[Wall, Pellet], allWalls 4, [Pellet], allWalls 5, [Pellet, Wall]]
        r4 = List.concat [[Wall, Pill], allWalls 4, [Pellet], allWalls 5, [Pellet, Wall]]
        r5 = r3
        r6 = List.concat [[Wall], allPellets 13]
        r7 = List.concat [[Wall, Pellet], allWalls 4, [Pellet], allWalls 2, [Pellet], allWalls 4]
        r8 = r7
        r9 = List.concat [[Wall], allPellets 6, allWalls 2, allPellets 4, [Wall]]
        r10 = List.concat [allWalls 6, [Pellet], allWalls 5, [Empty, Wall]]
        r11 = List.concat [allEmpty 5, [Wall, Pellet], allWalls 5, [Empty, Wall]]
        r12 = List.concat [allEmpty 5, [Wall, Pellet], allWalls 2, allEmpty 5]
        r13 = List.concat [allEmpty 5, [Wall, Pellet], allWalls 2, [Empty], allWalls 4]
        r14 = List.concat [allWalls 6, [Pellet], allWalls 2, [Empty, Wall], allEmpty 3]
        r15 = List.concat [allEmpty 6, [Pellet], allEmpty 3, [Wall], allEmpty 3]
        r16 = r14
        r17 = r13
        r18 = r12
        r19 = List.concat [allEmpty 5, [Wall, Pellet], allWalls 2, [Empty], allWalls 4]
        r20 =  List.concat [allWalls 6, [Pellet], allWalls 2, [Empty], allWalls 4]
        r21 = List.concat [[Wall], allPellets 12, [Wall]]
        r22 = r3
        r23 = r3
        r24 = List.concat [[Wall, Pill], allPellets 2, allWalls 2, allPellets 7, [Empty]]
        r25 = List.concat [allWalls 3, [Pellet], allWalls 2, [Pellet], allWalls 2, [Pellet], allWalls 4]
        r26 = r25
        r27 = r9
        r28 = List.concat [[Wall, Pellet], allWalls 10, [Pellet, Wall]]
        r29 = r28
        r30 = r6
        r31 = r1
        mapper half = List.append half (List.reverse half)
        rows = [r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16,r17,r18,r19,
                  r20,r21,r22,r23,r24,r25,r26,r27,r28,r29,r30,r31]
    in
      List.map mapper rows

initPacman : Pacman
initPacman =
    {
      pos=(0,-9),
      dir=Left
    }

initGhost : Pos -> Pos -> Ghost
initGhost start target =
    { pos=start,
      mode=Scatter,
      target=target,
      self=Normal
    }

initState : State
initState =
    { points=0,
      extraLives=2,
      gameState=Loading,
      board=initBoard,
      pacman=initPacman,
      blinky=(initGhost (0,3) (12,18)),
      inky=(initGhost (-2.5,0) (14,-17)),
      pinky=(initGhost (0,0) (-12,18)),
      clyde=(initGhost (2.5,0) (-14,-17))
    }



-- View

title w h =
    El.container w (h + 20) El.middle
          <| El.flow El.down
                 [El.image w h "/pacman-logo.jpg", El.spacer w 20]

renderPacman : Pacman -> Int -> El.Element
renderPacman p bSide =
    let
        pman_form =
            case p.dir of
              Left -> M.pacman ((toFloat bSide) / 2) M.Left
              Right -> M.pacman ((toFloat bSide) / 2) M.Right
              Up -> M.pacman ((toFloat bSide) / 2) M.Up
              Down -> M.pacman ((toFloat bSide) / 2) M.Down
    in
      Clg.collage bSide bSide [pman_form]

testerFun : Box -> Int -> El.Element
testerFun b bSide =
    case b of
      Empty -> Clg.collage bSide bSide [M.emptySpace (toFloat bSide)]
      Pellet -> Clg.collage bSide bSide [M.pellet ((toFloat bSide) / 6)]
      Pill -> Clg.collage bSide bSide [M.pill ((toFloat bSide) / 3)]
      Wall -> Clg.collage bSide bSide [M.wall (toFloat bSide)]

hspace = El.spacer 10

view : (Int, Int) -> El.Element
view (w, h) =
    let
        bSide = (h - 45) // 36
        titleHeight = 30
        titleWidth = bSide * 23
        ttl = title titleWidth titleHeight
        rowBuilder bxs = El.flow El.left (List.map (\b -> testerFun b bSide) bxs)
        colBuilder rws = El.flow El.down ([ttl] ++ rws)

    in
      El.color black
            <| El.container w h El.middle
            <| colBuilder (List.map rowBuilder initBoard)


-- Controller
main : Signal El.Element
main = view <~ Window.dimensions
