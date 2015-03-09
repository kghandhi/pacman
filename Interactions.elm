module Interactions where

import Pacman (..)
import List

pif (x, y) = (round x, round y)

-- Produces the ghost pacman overlaps with
overlap: State -> (List Ghost, List Ghost)
overlap st =
    let
        pacPos   = pif st.pacman.pos
        mapper g = (pif g.pos == pacPos) && (g.self /= Dead)
        parter g = g.self == Normal
    in
      List.partition parter
              <| List.filter mapper [st.blinky, st.pinky, st.inky, st.clyde]


makeEyes g scared =
    if List.member g scared then {g | self <- Dead
                                    , mode <- House}
    else g

interact : State -> State
interact st =
    let
        (scary, scared) = overlap st
        numScared       = List.length scared
        killPoints      = List.sum <| List.take numScared st.ghostPoints
        lifeLoss        = not <| List.isEmpty scary
        livesLeft       = if lifeLoss then st.extraLives-1 else st.extraLives
    in
      case (scary, scared) of
        ([], []) -> st
        _ -> {st | points  <- st.points + killPoints
                 , extraLives  <- livesLeft
                 , gameState   <- if | not lifeLoss  -> Active
                                     | livesLeft < 0 -> Over
                                     | otherwise     -> Loading
                 , blinky      <- makeEyes st.blinky scared
                 , pinky       <- makeEyes st.pinky scared
                 , inky        <- makeEyes st.inky scared
                 , clyde       <- makeEyes st.clyde scared
                 , ghostPoints <- List.drop numScared st.ghostPoints}
