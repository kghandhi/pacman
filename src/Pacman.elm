module Pacman where

import PacModels as M
import List exposing ((::))
import List
import Array as A
import Random as R

import Color exposing (..)
import Window
import Mouse
import Text as Txt
import Graphics.Element as El
import Graphics.Collage as Clg
import String

type alias Pos = (Float, Float)

-- Model
pelletPoint = 10
pillPoint = 50
cherryPoint = 100
ghostPoints = [200, 400, 800, 1600, 3000]
fleeTime = 6
totPells = 240
totPills = 4
dyingStates = 9

type alias Timers =
  {
    gameTimer : Float,
    startTimer : Float,
    fleeTimer : Float,
    fleeTimerOn : Bool,
    overTimer : Float,
    ghostSoundTimer : Float,
    pelletSoundTimer : Float
  }

type alias SoundControls =
  { 
    dying : Bool,
    intro : Bool,
    eatGhost : Bool,
    eatPellet : Bool
  }

type alias State =
  { 
    points : Int,
    extraLives : Int,
    gameState : GameState,
    board : Board,
    pacman : Pacman,
    blinky : Ghost,
    inky : Ghost,
    pinky : Ghost,
    clyde : Ghost,
    pellsAte : Int,
    pillsAte : Int,
    timers : Timers,
    ghostPoints : List Int,
    modeChanges : List Float,
    defaultMode : Mode,
    dyingList : Int,
    soundControls : SoundControls,
    raviMode : Bool,
    level : Int
  }

type Dir = Left | Right | Up | Down

type alias SpdMultis =
  { norm : Float,
    scrd : Float
  }

type alias Pacman =
  { 
    pos : Pos,
    prvPos : Pos,
    dir : Dir,
    spdMultis : SpdMultis,
    difMulti : Float
  }

-- Chase: kill pacman, Flee: pacman can eat me, Scatter : circle/idle
type Mode = Chase | Flee | Scatter | House | Inactive | Center
-- Flee -> Scared. Dead -> Eye appearance
type Self = Normal | Scared | Dead

type alias Ghost =
  { 
    name : String,
    pos : Pos,
    prvPos : Pos,
    dir : Dir,
    mode : Mode,
    target : Pos,
    self : Self,
    seed : R.Seed,
    spdMultis : SpdMultis,
    difMulti : Float
  }

type Box = Wall | Gate | Pellet | Empty | Fruit | Pill
type alias Row  = List Box
type alias Board = List Row

--states are start menu, preparing to start game, game active, pacman dying, game over, and game over menu
type GameState = Start | OptMenu | Loading | Active | Dying | Over | Over2

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
    r20 = List.concat [allWalls 6, [Pellet], allWalls 2, [Empty], allWalls 4]
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
-- The following two functions are meant only to check the sanity of the above.
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

initPacSpdMultis : SpdMultis
initPacSpdMultis =
  { 
    norm = 1,
    scrd = 1
  }

initPacman : Pacman
initPacman =
  {
    pos = (13.5, 23),
    prvPos = (13.5, 23),
    dir = Left,
    spdMultis = initPacSpdMultis,
    difMulti = 1
  }

initGhostSpdMultis : SpdMultis
initGhostSpdMultis =
  { 
    norm = 1,
    scrd = 0.5
  }

initBlinky : Ghost
initBlinky =
  { 
    name = "blinky",
    pos = (13.5, 11),
    prvPos = (13.5, 11),
    dir = Right,
    mode = Scatter,
    target = (25, -3),
    self = Normal,
    seed = R.initialSeed 13,
    spdMultis = initGhostSpdMultis,
    difMulti = 1
  }

initInky : Ghost
initInky =
  { 
    name = "inky",
    pos = (11.5, 14),
    prvPos = (11.5, 14),
    dir = Left,
    mode = Inactive,
    target = (27, 32),
    self = Normal,
    seed = R.initialSeed 17,
    spdMultis = initGhostSpdMultis,
    difMulti = 1
  }

initPinky : Ghost
initPinky =
  { 
    name = "pinky",
    pos = (13.5, 14),
    prvPos = (13.5, 14),
    dir = Left,
    mode = House,
    target = (2, -3),
    self = Normal,
    seed = R.initialSeed 19,
    spdMultis = initGhostSpdMultis,
    difMulti = 1
  }

initClyde : Ghost
initClyde =
  { 
    name = "clyde",
    pos = (15.5, 14),
    prvPos = (15.5, 14),
    dir = Left,
    mode = Inactive,
    target = (0, 32),
    self = Normal,
    seed = R.initialSeed 7,
    spdMultis = initGhostSpdMultis,
    difMulti = 1
  }

initTimers : Timers
initTimers =
  { 
    gameTimer = 0,
    startTimer = 4.0,
    fleeTimer = 0,
    fleeTimerOn = False,
    overTimer = 1.5,
    ghostSoundTimer = 0,
    pelletSoundTimer = 0
  }

initSounds : SoundControls
initSounds =
  { 
    dying = False,
    intro = False,
    eatGhost = False,
    eatPellet = False
  }

initState : State
initState =
  { 
    points = 0,
    extraLives = 2,
    gameState = Start,
    board = initBoard,
    pacman = initPacman,
    blinky = initBlinky,
    inky = initInky,
    pinky = initPinky,
    clyde = initClyde,
    pellsAte = 0,
    pillsAte = 0,
    timers = initTimers,
    ghostPoints = [200, 400, 800, 1600, 3000],
    modeChanges = [7, 27, 34, 54, 59, 79, 84],
    defaultMode = Scatter,
    dyingList = 9,
    soundControls = initSounds,
    raviMode = False,
    level = 1
  }
