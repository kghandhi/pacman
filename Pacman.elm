module Pacman where

import Models as M
import List ((::))
import List ((++))
import List
import Array as A
import Random as R

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

fleeTime = 6
totPells = 240

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
      numCaught : Int,
      pellsAte : Int,
      -- counts game time
      timer    : Float,
      fleeTimer : Float,
      fleeTimerOn : Bool,
      modeChanges : List Float,
      defaultMode : Mode
    }

type Dir = Left | Right | Up | Down

type alias Pacman =
    { pos : Pos,
      dir : Dir
    }

-- Chase: kill pacman, Flee: pacman can eat me, Scatter : circle/idle
type Mode = Chase | Flee | Scatter | House | Inactive
-- Flee -> Scared. Dead -> Eye appearance
type Self = Normal | Scared | Dead

type alias Ghost =
    { name : String,
      pos : Pos,
      dir : Dir,
      mode : Mode,
      target : Pos,
      self : Self,
      seed : R.Seed
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

-- 240
countRow bs =
    let
        folder b acc =
            case b of
              Pellet -> 1 + acc
              _ -> acc
    in
      List.foldl folder 0 bs

countPellets: List (List Box) -> Int
countPellets board =
    List.foldl (\bs acc -> acc + (countRow bs)) 0 board

initPacman : Pacman
initPacman =
    {
      pos=(13.5, 23),
      dir=Left
    }

initGhost : String -> Dir -> Pos -> Pos -> Ghost
initGhost n d start target =
    { name=n,
      pos=start,
      dir=d,
      mode=Scatter,
      target=target,
      self=Normal,
      seed=(R.initialSeed 13)
    }

initBlinky : Ghost
initBlinky =
    { name   = "blinky",
      pos    = (13, 11),
      dir    = Right,
      mode   = Scatter,
      target = (25, -3),
      self   = Normal,
      seed   = R.initialSeed 13
    }

initInky : Ghost
initInky =
    { name   = "inky",
      pos    = (11, 14),
      dir    = Left,
      mode   = Inactive,
      target = (27, 32),
      self   = Normal,
      seed   = R.initialSeed 17
    }

initPinky : Ghost
initPinky =
    { name   = "pinky",
      pos    = (13, 14),
      dir    = Left,
      mode   = House,
      target = (2, -3),
      self   = Normal,
      seed   = R.initialSeed 19
    }

initClyde : Ghost
initClyde =
    { name   = "clyde",
      pos    = (15, 14),
      dir    = Left,
      mode   = Inactive,
      target = (0, 32),
      self   = Normal,
      seed   = R.initialSeed 7
    }

initState : State
initState =
    { points      = 0,
      extraLives  = 2,
      gameState   = Loading,
      board       = initBoard,
      pacman      = initPacman,
      blinky      = initBlinky,
      inky        = initInky,
      pinky       = initPinky,
      clyde       = initClyde,
      numCaught   = 0,
      pellsAte    = 0,
      timer       = 0,
      fleeTimer   = 0,
      fleeTimerOn = False,
      modeChanges = [7, 27, 34, 54, 59, 79, 84],
      defaultMode = Scatter
    }
