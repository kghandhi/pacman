module Display where

import Pacman (..)
import Controls      as Ctr
import BoardControls as BCtr
import GhostControls as GCtr
import Interactions  as Itr
import PacModels     as Mod
import Utils         as Utl
import Time (..)
import Audio as Aud
import Audio (defaultTriggers)

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
import Graphics.Input   as Inp
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
              Left  -> Mod.pacman Mod.Left ((toFloat bSide) / 2)
              Right -> Mod.pacman Mod.Right ((toFloat bSide) / 2)
              Up    -> Mod.pacman Mod.Up ((toFloat bSide) / 2)
              Down  -> Mod.pacman Mod.Down ((toFloat bSide) / 2)
    in
      Clg.collage bSide bSide [pman_form]

displayBox : Box -> Int -> El.Element
displayBox b bSide =
    case b of
      Empty  -> Clg.collage bSide bSide [Mod.emptySpace (toFloat bSide)]
      Pellet -> Clg.collage bSide bSide [Mod.pellet ((toFloat bSide) / 6)]
      Pill   -> Clg.collage bSide bSide [Mod.pill ((toFloat bSide) / 3)]
      Wall   -> Clg.collage bSide bSide [Mod.wall (toFloat bSide)]
      Gate   -> Clg.collage bSide bSide [Mod.gate (toFloat bSide)]

displayLives : Int -> Int -> El.Element
displayLives lives sz =
    let
        life = Clg.collage sz sz [Mod.pacman Mod.Right ((toFloat sz) / 2)]
    in
      El.flow El.left (List.map (\_ -> life) [1..lives])

magicNumber = 45

scoreStyle : Txt.Style
scoreStyle = {typeface = font
             , height  = Just magicNumber
             , color   = white
             , bold    = False
             , italic  = False
             , line    = Nothing
             }

renderGhost g bSide =
    let
        h = toFloat bSide
        w = h
    in
      case g.self of
        Scared -> Mod.ghost "scared" w h
        Dead   -> Mod.ghost "dead" w h
        Normal -> Mod.ghost g.name w h

myButton : ButtonPressed -> String -> Float -> Float -> Clg.Form
myButton msg str w h =
  let
    button_general c str =
      Clg.collage (round w) (round h)
        [ Clg.filled black <| Clg.rect (0.95 * w)  (0.96 * h)
        , Clg.filled c     <| Clg.rect (0.92 * w)  (0.9  * h)
        , Clg.toForm       <| Txt.centered
                           <| Txt.color black
                           <| Txt.height (h * 0.45)
                           <| Txt.typeface font
                           <| Txt.fromString str
        ]
    button_up    str = button_general yellow      str
    button_hover str = button_general lightYellow str
    button_down  str = button_general darkYellow  str
  in
    Clg.toForm
        <| Inp.customButton
            (Signal.send actionChannel (ButtonAction msg))
            (button_up    str)
            (button_hover str)
            (button_down  str)

menuBackground : Float -> Float -> Clg.Form
menuBackground w h =
  let
    lin_stl  = Clg.solid darkYellow
    lin_stl' = {lin_stl | width <- 7}
  in
    Clg.group
      [ Clg.filled darkBlue   <| Clg.rect w h
      , Clg.outlined lin_stl' <| Clg.rect w h
      ]

logoBox : Float -> Float -> Float -> Float -> Clg.Form
logoBox logoOffset ovalW ovalH textH =
  Clg.moveY logoOffset
    <| Clg.group
         [ Clg.filled yellow  <| Clg.oval (1.05 * ovalW) (1.05 * ovalH)
         , Clg.filled black   <| Clg.oval ovalW ovalH
         , Clg.toForm         <| Txt.centered
                              <| Txt.color yellow
                              <| Txt.height  textH
                              <| Txt.typeface font
                              <| Txt.fromString "Pac-Man"
         ]

playMenu : Int -> Int -> State -> String -> Clg.Form
playMenu w h st startStr =
  let
    (fw, fh) = (toFloat w, toFloat h)
    el_wt = fw * 0.8
    el_ht = fh / 5
  in
    Clg.toForm
      <| Clg.collage w h
           [ menuBackground fw fh
           , logoBox (fh / 4) el_wt el_ht (fh / 8)
           , Clg.moveY (-fh / 15)       <| myButton Go startStr  el_wt el_ht
           , Clg.moveY (-4.3 * fh / 15) <| myButton Go "Options" el_wt el_ht
           ]

startMenu : Int -> Int -> State -> Clg.Form
startMenu w h st =
  playMenu w h st "Start"

overMenu : Int -> Int -> State -> Clg.Form
overMenu w h st =
  playMenu w h st "Play Again?"

view : (Int, Int) -> State -> El.Element
view (w, h) st =
    let
        bSide       = (h - magicNumber) // 36
        titleHeight = 30
        titleWidth  = bSide * 23

        ttl = title titleWidth titleHeight

        score = "SCORE: " ++ (toString st.points)
              |> Txt.fromString
              |> Txt.style scoreStyle
              |> Txt.leftAligned
        scoreLives = El.flow El.right [score, (displayLives st.extraLives magicNumber)]

        gState_pos = Utl.itow (bSide * numCols) (titleHeight + 20 + (bSide * numRows)) (13.5, 17)
        gState = case st.gameState of
                   Loading -> El.fittedImage (6 * bSide) bSide "/loading.png"
                   Over    -> El.fittedImage (8 * bSide) bSide "/over.png"
                   _       -> El.empty

        rowBuilder bxs = El.flow El.left (List.map (\b -> displayBox b bSide) bxs)
        colBuilder rws = El.flow El.down ([scoreLives] ++ rws)

        wFromBxs = bSide * numCols
        hFromBxs = titleHeight + 20 + (bSide * numRows)

        pinky_pos  = Utl.itow wFromBxs hFromBxs st.pinky.pos
        inky_pos   = Utl.itow wFromBxs hFromBxs st.inky.pos
        blinky_pos = Utl.itow wFromBxs hFromBxs st.blinky.pos
        clyde_pos  = Utl.itow wFromBxs hFromBxs st.clyde.pos
        pac_pos    = Utl.itow wFromBxs hFromBxs st.pacman.pos
        pac_dir    = case st.pacman.dir of
                       Left  -> Mod.Left
                       Right -> Mod.Right
                       Up    -> Mod.Up
                       Down  -> Mod.Down
        pacSelf = case st.gameState of
                    Dying -> let self = Mod.animatePacman pac_dir (toFloat (bSide // 2)) in
                             List.head <| List.drop (dyingStates - st.dyingList) self
                    _ -> Mod.pacman pac_dir <| toFloat <| bSide // 2
        ghosts = if st.gameState == Dying then Clg.toForm El.empty
                 else Clg.group [Clg.move pinky_pos <| renderGhost st.pinky bSide
                                , Clg.move inky_pos <| renderGhost st.inky bSide
                                , Clg.move blinky_pos <| renderGhost st.blinky bSide
                                , Clg.move clyde_pos <| renderGhost st.clyde bSide]
    in
      El.color black
            <| Clg.collage w h
                 [ Clg.toForm          <| colBuilder (List.map rowBuilder st.board)
                 , Clg.move pac_pos    <| pacSelf
                 , ghosts
                 , Clg.move gState_pos <| Clg.toForm gState
                 , if  | st.gameState == Start -> startMenu (min w 350) (min h 400) st
                       | st.gameState == Over2 -> overMenu  (min w 350) (min h 400) st
                       | otherwise             -> Clg.toForm El.empty
                 ]

--Controller

type ButtonPressed = Go
type Action = KeyAction Key.KeyCode | TimeAction | ButtonAction ButtonPressed

actionChannel : Signal.Channel Action
actionChannel =
  Signal.channel <| ButtonAction Go

actions : Signal Action
actions =
  Signal.mergeMany
    [ (Signal.map (\k -> KeyAction k) Key.lastPressed)
    , (Signal.sampleOn (fps 40)  <| Signal.constant TimeAction)
    , (Signal.subscribe actionChannel)
    ]



currState : Signal State
currState =
  let
    zeroedTimers =
      { gameTimer   = 0,
        startTimer  = 0,
        fleeTimer   = 0,
        fleeTimerOn = False,
        overTimer   = 0
      }
  in
    Signal.dropRepeats
      <| Signal.map (\s -> {s | pellsAte    <- 0,
                                timers      <- zeroedTimers,
                                ghostPoints <- [],
                                modeChanges <- [],
                                defaultMode <- Chase})
      <| Signal.foldp upstate initState actions

allNormal : State -> Bool
allNormal s =
  List.all (\m -> m == Normal) [s.blinky.self, s.pinky.self, s.inky.self, s.clyde.self]

upstate : Action -> State -> State
upstate a s =
  case (a, s.gameState) of
    (TimeAction, Dying) ->
        if | s.dyingList  > 0 -> {s | dyingList     <- s.dyingList - 1}
           | s.extraLives < 0 -> {s | gameState     <- Over}
           | otherwise        -> {s | dyingList     <- dyingStates, 
                                      gameState     <- Loading,
                                      pacman        <- initPacman,
                                      blinky        <- initBlinky,
                                      pinky         <- initPinky,
                                      inky          <- initInky,
                                      clyde         <- initClyde}
    (TimeAction, Loading) ->
      let
        newStartTimer    = s.timers.startTimer - 0.025
        newSTLess0       = newStartTimer < 0
        tmers            = s.timers
        newTimers        = 
          {tmers | startTimer <- if | newSTLess0 -> initState.timers.startTimer
                                    | otherwise  -> newStartTimer}
        sCs              = s.soundControls
        newSoundControls = {sCs | dying <- False}
      in
        {s | timers        <- newTimers,
             gameState     <- if newSTLess0 then Active else Loading,
             pacman        <- initPacman,
             blinky        <- initBlinky,
             pinky         <- initPinky,
             inky          <- initInky,
             clyde         <- initClyde,
             soundControls <- if newSTLess0 then newSoundControls else s.soundControls}
    (KeyAction k, Active) -> {s | pacman <- Ctr.updateDir  k s.pacman}
    (TimeAction, Active)  ->
      let
        (extra_pts, newBoard) = BCtr.updateBoard s.board s.pacman
        old_pts      = s.points
        atePill      = extra_pts == pillPoint
        atePell      = extra_pts == pelletPoint
        old_pellsAte = s.pellsAte
        stopFlee     = s.timers.fleeTimer >= fleeTime
        tmers        = s.timers
        newTimers    =
          {tmers | gameTimer  <- tmers.gameTimer + (if tmers.fleeTimer > 0 then 0 else 0.025)
                 , fleeTimer  <- if | stopFlee
                                        || not s.timers.fleeTimerOn
                                        || atePill -> 0
                                    | otherwise    -> tmers.fleeTimer + 0.025
                 , fleeTimerOn <- if | stopFlee  -> False
                                     | atePill   -> True
                                     | otherwise -> tmers.fleeTimerOn}
      in
        Itr.interact
          <| GCtr.updateGhosts
               {s | pacman      <- Ctr.updatePacPos s.pacman
                  , board       <- newBoard
                  , points      <- old_pts + extra_pts
                  , pellsAte    <- old_pellsAte + (if atePell then 1 else 0)
                  , timers      <- newTimers
                  , ghostPoints <- if atePill then ghostPoints else s.ghostPoints} atePill
    (TimeAction, Over)   ->
      if | s.timers.overTimer <= 0 -> 
             let
               sCs = s.soundControls
               newSoundControls = {sCs | dying <- False}
             in
               {s | gameState <- Over2, soundControls <- newSoundControls}
         | otherwise               ->
             let
               tmers = s.timers
               newTimers = {tmers | overTimer <- tmers.overTimer - 0.025}
            in
             {s | timers <- newTimers}
    (ButtonAction Go, _) -> {initState | gameState <- Loading}
    _ -> s

{- note from Abe and Kira, much of the audio code below was written using
   https://github.com/jcollard/elm-audio/blob/master/AudioTest.elm
   as a reference -}

propertiesHandler : Aud.Properties -> Maybe Aud.Action
propertiesHandler props =
  if props.currentTime > props.duration
  then Just <| Aud.Pause
  else Nothing

handleAudio : State -> Aud.Action
handleAudio st =
  if st.soundControls.dying
  then Aud.Play
  else Aud.Pause

builder : Signal (Aud.Event, Aud.Properties)
builder = Aud.audio { src = "/PacManDies.wav",
                      triggers = {defaultTriggers | timeupdate <- True},
                      propertiesHandler = propertiesHandler,
                      actions = handleAudio <~ currState}

main : Signal El.Element
main = view <~ Window.dimensions ~ currState