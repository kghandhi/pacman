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
pelletPoint = 10
pillPoint = 50
ghostPoints = [200, 400, 800, 1600, 3000]
cherryPoint = 100

type alias State =
    { points : Int,
      extraLives : Int,
      gameState : GameState,
      board : Board,
      pacman : Pacman,
      blinky : Ghost,
      inky : Ghost,
      pinky : Ghost,
      clyde : Ghost,
      numCaught : Int
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
    { name : String,
      pos : Pos,
      dir : Dir,
      mode : Mode,
      target : Pos,
      self : Self
    }

type Box = Wall | Gate | Pellet | Empty | Fruit | Pill
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
        r13 = List.concat [allEmpty 5, [Wall, Pellet], allWalls 2, [Empty], allWalls 3, [Gate]]
        r14 = List.concat [allWalls 6, [Pellet], allWalls 2, [Empty, Wall], allEmpty 3]
        r15 = List.concat [allEmpty 6, [Pellet], allEmpty 3, [Wall], allEmpty 3]
        r16 = r14
        r17 = List.concat [allEmpty 5, [Wall, Pellet], allWalls 2, [Empty], allWalls 4]
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
      pos=(13.5, 23),
      dir=Left
    }

initGhost : String -> Pos -> Pos -> Ghost
initGhost n start target =
    { name=n,
      pos=start,
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
      blinky=(initGhost "blinky" (13,11) (0,27)),
      inky=(initGhost "inky" (11,14) (30,27)),
      pinky=(initGhost "pinky" (13,14) (0,0)),
      clyde=(initGhost "clyde" (15,14) (0,30)),
      numCaught=0
    }
