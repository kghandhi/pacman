module Interactions where

import Pacman exposing (..)
import List

pif (x, y) = (round x, round y)

sidesSwapped : Ghost -> Pacman -> Bool
sidesSwapped g p =
  let
    (gx, gy)   = g.pos
    (px, py)   = p.pos
    (gpx, gpy) = g.prvPos
    (ppx, ppy) = p.prvPos
  in
    (gx == px && gpx == ppx && ((gy <= py && gpy >= ppy) || (gy >= py && gpy <= ppy))) ||
    (gy == py && gpy == ppy && ((gx <= px && gpx >= ppx) || (gx >= px && gpx <= ppx)))


-- Produces the ghost pacman overlaps with
overlap: State -> (List Ghost, List Ghost)
overlap st =
  let
    pacPos   = pif st.pacman.pos
    mapper g = ((sidesSwapped g st.pacman) || (pif g.pos == pacPos)) && (g.self /= Dead)
    parter g = g.self == Normal
  in
    List.partition parter
      <| List.filter mapper [st.blinky, st.pinky, st.inky, st.clyde]


makeEyes g scared =
  if List.member g scared then {g | self = Dead
                                  , mode = House}
  else g

interact : State -> State
interact st =
  let
    (scary, scared) = overlap st
    numScared       = List.length scared
    killPoints      = List.sum <| List.take numScared st.ghostPoints
    lifeLoss        = not <| List.isEmpty scary
    livesLeft       = if lifeLoss then st.extraLives - 1 else st.extraLives
    tmers           = st.timers
    newTimers       = {tmers | ghostSoundTimer = if numScared > 0 then 0.15 else tmers.ghostSoundTimer - 0.025}
    sCs             = st.soundControls
    newSoundControls = {sCs | eatGhost = newTimers.ghostSoundTimer > 0}
    st'             = {st | timers = newTimers, soundControls = newSoundControls}
  in
    case (scary, scared) of
      ([], []) -> st'
      _ -> {st' | points        = st.points + killPoints
                , extraLives    = livesLeft
                , gameState     = if not lifeLoss then Active else Dying
                , blinky        = makeEyes st.blinky scared
                , pinky         = makeEyes st.pinky scared
                , inky          = makeEyes st.inky scared
                , clyde         = makeEyes st.clyde scared
                , ghostPoints   = List.drop numScared st.ghostPoints
                , soundControls =
                    let
                        newSoundControls' =
                          {newSoundControls | dying = lifeLoss}
                    in
                        newSoundControls'}
