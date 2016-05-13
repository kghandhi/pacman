module Display where

import Pacman exposing (..)
import Controls      as Ctr
import BoardControls as BCtr
import GhostControls as GCtr
import Interactions  as Itr
import PacModels     as Mod
import Utils         as Utl
import Time exposing (..)
import Audio as Aud
import Audio exposing (defaultTriggers)

import List exposing ((::))
import List
import Array as A

import Color exposing (..)
import Signal
import Signal.Extra exposing (andMap)
import Window
import Mouse
import Text as Txt
import Graphics.Element as El
import Graphics.Collage as Clg
import Graphics.Input   as Inp
import String
import Keyboard as Key
import Char exposing (KeyCode)

unsafeFromJust : Maybe a -> a
unsafeFromJust maybe =
  case maybe of
    Just value -> value
    Nothing -> Debug.crash "This shouldn't happen"

-- View
font = ["Andale Mono", "monospace"]

title w h =
    El.container w (h + 20) El.middle
          <| El.flow El.down
                 [El.image w h "/graphics/pacman-logo.jpg", El.spacer w 20]

-- renderPacman : Pacman -> Int -> El.Element
-- renderPacman p bSide =
--     let
--         pman_form =
--             case p.dir of
--               Left  -> Mod.pacman Mod.Left ((toFloat bSide) / 2)
--               Right -> Mod.pacman Mod.Right ((toFloat bSide) / 2)
--               Up    -> Mod.pacman Mod.Up ((toFloat bSide) / 2)
--               Down  -> Mod.pacman Mod.Down ((toFloat bSide) / 2)
--     in
--       Clg.collage bSide bSide [pman_form]

displayBox : Box -> Int -> El.Element
displayBox b bSide =
    case b of
      Empty  -> Clg.collage bSide bSide [Mod.emptySpace (toFloat bSide)]
      Pellet -> Clg.collage bSide bSide [Mod.pellet ((toFloat bSide) / 6)]
      Pill   -> Clg.collage bSide bSide [Mod.pill ((toFloat bSide) / 3)]
      Wall   -> Clg.collage bSide bSide [Mod.wall (toFloat bSide)]
      Gate   -> Clg.collage bSide bSide [Mod.gate (toFloat bSide)]
      Fruit  -> Clg.collage bSide bSide [Mod.fruit]

displayLives : Int -> Int -> Bool -> El.Element
displayLives lives sz ravi =
    let
        life = Clg.collage sz sz [Mod.pacman Mod.Right ((toFloat sz) / 2) ravi]
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

renderGhost g bSide ravi =
    let
        h = toFloat bSide
        w = h
    in
      case g.self of
        Scared -> Mod.ghost "scared" w h ravi
        Dead   -> Mod.ghost "dead" w h ravi
        Normal -> Mod.ghost g.name w h ravi

myButton : ButtonPressed -> String -> Float -> Float -> Clg.Form
myButton msg str w h =
  let
    button_general c str =
      Clg.collage (round w) (round h)
        [ Clg.filled black <| Clg.rect (0.95 * w)  (0.96 * h)
        , Clg.filled c     <| Clg.rect (0.92 * w)  (0.9  * h)
        , Clg.toForm       <| El.centered
                           <| Txt.color black
                           <| Txt.height (w * 0.13)
                           <| Txt.typeface font
                           <| Txt.fromString str
        ]
    button_up    str = button_general yellow      str
    button_hover str = button_general lightYellow str
    button_down  str = button_general darkYellow  str
  in
    Clg.toForm
        <| Inp.customButton
            (Signal.message actionMailbox.address (ButtonAction msg))
            (button_up    str)
            (button_hover str)
            (button_down  str)

menuBackground : Float -> Float -> Clg.Form
menuBackground w h =
  let
    lin_stl  = Clg.solid darkYellow
    lin_stl' = {lin_stl | width = 7}
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
         , Clg.toForm         <| El.centered
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
           , Clg.moveY (-fh / 15)       <| myButton Go      startStr  el_wt el_ht
           , Clg.moveY (-4.3 * fh / 15) <| myButton Options "Options" el_wt el_ht
           ]

startMenu : Int -> Int -> State -> Clg.Form
startMenu w h st =
  playMenu w h st "Start"

overMenu : Int -> Int -> State -> Clg.Form
overMenu w h st =
  playMenu w h st "Play Again?"

optionsMenu : Int -> Int -> State -> Clg.Form
optionsMenu w h st =
  let
    (fw, fh) = (toFloat w, toFloat h)
    el_wt = fw * 0.8
    el_ht = fh / 5
  in
    Clg.toForm
      <| Clg.collage w h
           [ menuBackground fw fh
           , logoBox (fh / 4) el_wt el_ht (fh / 8)
           , Clg.move ((-0.9 * fw / 4), (-5.85 * fh / 15)) <|
               myButton Go "Play Game"  (el_wt / 2) (2 * el_ht / 3)
           , Clg.move (( 0.9 * fw / 4), (-5.85 * fh / 15)) <|
              myButton Strt "Go Back"  (el_wt / 2) (2 * el_ht / 3)
           , Clg.move (-0.74 * fw / 4, (fh / 18))
                    <| Clg.toForm
                    <| El.centered
                    <| Txt.color yellow
                    <| Txt.height (fw * 0.08)
                    <| Txt.typeface font
                    <| Txt.fromString "Mode"
            , Clg.move ((0.74 * fw / 4), (fh / 18))
                    <| Clg.toForm
                    <| El.width (w // 3)
                    <| Inp.dropDown (\b -> Signal.message actionMailbox.address <| ButtonAction <| PicMode b)
                         [ ("Game Modes", Nothing   )
                         , ("Normal"    , Just False)
                         , ("CS223"     , Just True )
                         ]
           ]

view : (Int, Int) -> State -> El.Element
view (w, h) st =
    let
        ravi = st.raviMode
        bSide       = (h - magicNumber) // 36
        titleHeight = 30
        titleWidth  = bSide * 23

        ttl = title titleWidth titleHeight

        score = "SCORE: " ++ (toString st.points)
              |> Txt.fromString
              |> Txt.style scoreStyle
              |> El.leftAligned
        scoreLives = El.flow El.right [score, (displayLives st.extraLives magicNumber ravi)]

        gState_pos = Utl.itow (bSide * numCols) (titleHeight + 20 + (bSide * numRows)) (13.5, 17)
        gState = case st.gameState of
                   Loading -> El.fittedImage (6 * bSide) bSide "/graphics/loading.png"
                   Over    -> El.fittedImage (8 * bSide) bSide "/graphics/over.png"
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
                    Dying ->
                        let
                            self = Mod.animatePacman pac_dir (toFloat (bSide // 2)) ravi
                        in
                          unsafeFromJust <| List.head <| List.drop (dyingStates - st.dyingList) self
                    _ -> (Mod.pacman pac_dir <| toFloat <| bSide // 2) ravi
        ghosts = if st.gameState == Dying then Clg.toForm El.empty
                 else Clg.group [Clg.move pinky_pos <| renderGhost st.pinky bSide ravi
                                , Clg.move inky_pos <| renderGhost st.inky bSide ravi
                                , Clg.move blinky_pos <| renderGhost st.blinky bSide ravi
                                , Clg.move clyde_pos <| renderGhost st.clyde bSide ravi]
    in
      El.color black
            <| Clg.collage w h
                 [ Clg.toForm          <| colBuilder (List.map rowBuilder st.board)
                 , Clg.move pac_pos    <| pacSelf
                 , ghosts
                 , Clg.move gState_pos <| Clg.toForm gState
                 , if st.gameState == Start then
                      startMenu   (min w 350) (min h 400) st
                   else if st.gameState == Over2 then
                      overMenu    (min w 350) (min h 400) st
                   else if st.gameState == OptMenu then
                      optionsMenu (min w 350) (min h 400) st
                   else
                      Clg.toForm El.empty
                 ]

--Controller

type ButtonPressed = Go | Options | Strt | PicMode (Maybe Bool)
type Action = KeyAction KeyCode | TimeAction | ButtonAction ButtonPressed

actionMailbox : Signal.Mailbox Action
actionMailbox =
  Signal.mailbox <| ButtonAction Go

actions : Signal Action
actions =
  Signal.mergeMany
    [ (Signal.map (\k -> KeyAction k) Key.presses)
    , (Signal.sampleOn (fps 40)  <| Signal.constant TimeAction)
    , (actionMailbox.signal)
    ]



currState : Signal State
currState =
  let
    zeroedTimers =
      { gameTimer        = 0,
        startTimer       = 0,
        fleeTimer        = 0,
        fleeTimerOn      = False,
        overTimer        = 0,
        ghostSoundTimer  = 0,
        pelletSoundTimer = 0
      }
  in
    Signal.dropRepeats
      <| Signal.map (\s -> {s | pellsAte    = 0,
                                pillsAte    = 0,
                                timers      = zeroedTimers,
                                ghostPoints = [],
                                modeChanges = [],
                                defaultMode = Chase})
      <| Signal.foldp upstate initState actions

allNormal : State -> Bool
allNormal s =
  List.all (\m -> m == Normal) [s.blinky.self, s.pinky.self, s.inky.self, s.clyde.self]

upstate : Action -> State -> State
upstate a s =
  case (a, s.gameState) of
    (TimeAction, Dying) ->
        if s.dyingList > 0 then
          {s | dyingList     = s.dyingList - 1}
        else if s.extraLives < 0 then
          {s | gameState     = Over}
        else
          {s | dyingList     = dyingStates,
               gameState     = Loading,
               pacman        = initPacman,
               blinky        = initBlinky,
               pinky         = initPinky,
               inky          = initInky,
               clyde         = initClyde}
    (TimeAction, Loading) ->
      let
        newStartTimer    = s.timers.startTimer - 0.025
        newSTLess0       = newStartTimer < 0
        tmers            = s.timers
        newTimers        =
          {tmers | startTimer = if newSTLess0 then initState.timers.startTimer / 2 else newStartTimer}
        sCs              = s.soundControls
        newSoundControls = {sCs | dying = False, intro = False}
      in
        {s | timers        = newTimers,
             gameState     = if newSTLess0 then Active else Loading,
             pacman        = initPacman,
             blinky        = initBlinky,
             pinky         = initPinky,
             inky          = initInky,
             clyde         = initClyde,
             soundControls = if newSTLess0 then newSoundControls else s.soundControls}
    (KeyAction k, Active) -> {s | pacman = Ctr.updateDir  k s.pacman}
    (TimeAction, Active)  ->
      let
        (extra_pts, newBoard) = BCtr.updateBoard s.board s.pacman
        old_pts      = s.points
        atePill      = extra_pts == pillPoint
        atePell      = extra_pts == pelletPoint
        old_pellsAte = s.pellsAte
        new_pellsAte = old_pellsAte + (if atePell then 1 else 0)
        old_pillsAte = s.pillsAte
        new_pillsAte = old_pillsAte + (if atePill then 1 else 0)
        level_done   = new_pillsAte + new_pellsAte == totPills + totPells
        stopFlee     = s.timers.fleeTimer >= fleeTime
        tmers        = s.timers
        newTimers    =
          {tmers | gameTimer   =
                     tmers.gameTimer + (if tmers.fleeTimer > 0 then 0 else 0.025)
                 , fleeTimer   =
                     if stopFlee || not s.timers.fleeTimerOn || atePill then
                        0
                     else
                        tmers.fleeTimer + 0.025
                 , fleeTimerOn =
                     if stopFlee then
                        False
                     else if atePill then
                        True
                     else
                        tmers.fleeTimerOn
                 , pelletSoundTimer =
                     if atePill || atePell then
                        0.3
                     else
                        tmers.pelletSoundTimer - 0.025}
        sCs              = s.soundControls
        newSoundControls = if atePill || atePell then
                              {sCs | eatPellet = True}
                           else if newTimers.pelletSoundTimer <= 0 then
                              {sCs | eatPellet = False}
                           else
                              sCs
        s' = {s | pacman        = Ctr.updatePacPos s.pacman newTimers.fleeTimerOn
                , board         = newBoard
                , points        = old_pts + extra_pts
                , pellsAte      = new_pellsAte
                , pillsAte      = new_pillsAte
                , timers        = newTimers
                , ghostPoints   = if atePill then ghostPoints else s.ghostPoints
                , soundControls = newSoundControls}
      in
        if level_done
        then
          {initState | points     = s'.points,
                       extraLives = s'.extraLives,
                       gameState  = Loading,
                       timers     = {initTimers | startTimer = initTimers.startTimer / 2},
                       raviMode   = s'.raviMode,
                       level      = s'.level + 1}
        else
          Itr.interact <| GCtr.updateGhosts s'

                atePill
    (TimeAction, Over)   ->
      if s.timers.overTimer <= 0 then
        let
          sCs = s.soundControls
          newSoundControls = {sCs | dying = False}
        in
          {s | gameState = Over2, soundControls = newSoundControls}
      else
        let
          tmers = s.timers
          newTimers = {tmers | overTimer = tmers.overTimer - 0.025}
        in
          {s | timers = newTimers}
    (ButtonAction Go, _) ->
      let
        sCs = s.soundControls
        newSoundControls = {sCs | intro = True}
      in
        {initState | gameState = Loading, soundControls = newSoundControls, raviMode = s.raviMode}
    (ButtonAction Options, _) -> {s | gameState = OptMenu}
    (ButtonAction Strt, _) -> {s | gameState = Start}
    (ButtonAction (PicMode (Just b)), _) -> {s | raviMode = b}
    _ -> s

{- note from Abe and Kira, much of the audio code below was written using
   https://github.com/jcollard/elm-audio/blob/master/AudioTest.elm
   as a reference -}

deathPropertiesHandler : Aud.Properties -> Maybe Aud.Action
deathPropertiesHandler props =
  if props.currentTime > props.duration
  then Just <| Aud.Pause
  else Nothing

deathHandleAudio : State -> Aud.Action
deathHandleAudio st =
  if st.soundControls.dying
  then Aud.Play
  else Aud.Pause

deathBuilder : Signal (Aud.Event, Aud.Properties)
deathBuilder = Aud.audio { src = "/sounds/PacManDies.wav",
                           triggers = {defaultTriggers | timeupdate = True},
                           propertiesHandler = deathPropertiesHandler,
                           actions = Signal.map deathHandleAudio currState}

introPropertiesHandler : Aud.Properties -> Maybe Aud.Action
introPropertiesHandler props =
  if props.currentTime > props.duration
  then Just <| Aud.Pause
  else Nothing

introHandleAudio : State -> Aud.Action
introHandleAudio st =
  if st.soundControls.intro
  then Aud.Play
  else Aud.Pause

introBuilder : Signal (Aud.Event, Aud.Properties)
introBuilder = Aud.audio { src = "/sounds/pacman_beginning.wav",
                           triggers = {defaultTriggers | timeupdate = True},
                           propertiesHandler = introPropertiesHandler,
                           actions = Signal.map introHandleAudio currState}

ghostPropertiesHandler : Aud.Properties -> Maybe Aud.Action
ghostPropertiesHandler props =
  if props.currentTime > props.duration
  then Just <| Aud.Pause
  else Nothing

ghostHandleAudio : State -> Aud.Action
ghostHandleAudio st =
  if st.soundControls.eatGhost
  then Aud.Play
  else Aud.Pause

ghostBuilder : Signal (Aud.Event, Aud.Properties)
ghostBuilder = Aud.audio { src = "/sounds/pacman_eatghost.wav",
                           triggers = {defaultTriggers | timeupdate = True},
                           propertiesHandler = ghostPropertiesHandler,
                           actions = Signal.map ghostHandleAudio currState}

pellPropertiesHandler : Aud.Properties -> Maybe Aud.Action
pellPropertiesHandler props =
  if props.currentTime > props.duration
  then Just <| Aud.Pause
  else Nothing

pellHandleAudio : State -> Aud.Action
pellHandleAudio st =
  if st.soundControls.eatPellet
  then Aud.Play
  else Aud.Pause

pellBuilder : Signal (Aud.Event, Aud.Properties)
pellBuilder = Aud.audio { src = "/sounds/pacman_chomp.wav",
                           triggers = {defaultTriggers | timeupdate = True},
                           propertiesHandler = pellPropertiesHandler,
                           actions = Signal.map pellHandleAudio currState}

main : Signal El.Element
--main = view <~ Window.dimensions ~ currState
main = view `Signal.map` Window.dimensions `andMap` currState
