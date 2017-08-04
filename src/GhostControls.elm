module GhostControls where

import Controls exposing (..)
import Random as R
import Pacman exposing (..)
import List as Lst

unsafeFromJust : Maybe a -> a
unsafeFromJust maybe =
  case maybe of
    Just value -> value
    Nothing -> Debug.crash "This shouldn't happen"

dist (x1, y1) (x2, y2) = sqrt ((x2-x1)^2 + (y2-y1)^2)

pfi : (Int, Int) -> Pos
pfi (x, y) = (toFloat x, toFloat y)

ghostPace : Float
ghostPace = 0.5

notHome : Ghost -> Bool
notHome g =
  (g.mode /= House) && (g.mode /= Center)

ghostActive : Ghost -> Bool
ghostActive g =
  (g.mode == Scatter) || (g.mode == Chase)

updateGhostPos : Ghost -> Pos -> Ghost
updateGhostPos g targ =
  let
    delta                  = ghostPace * g.difMulti
                                       * (if   g.self == Scared
                                          then g.spdMultis.scrd
                                          else g.spdMultis.norm)
    (gx, gy)               = g.pos
    curDirOrBarrier d      =
      case d of
        Left  -> g.dir /=  Right && (not <| isBarrier (gx - delta, gy        ) <| notHome g)
        Right -> g.dir /=  Left  && (not <| isBarrier (gx + delta, gy        ) <| notHome g)
        Up    -> g.dir /=  Down  && (not <| isBarrier (gx        , gy - delta) <| notHome g)
        Down  -> g.dir /=  Up    && (not <| isBarrier (gx        , gy + delta) <| notHome g)
    legal_dirs             = Lst.filter curDirOrBarrier [Left, Right, Up, Down]
    distToTarg pos         =
      dist targ pos

    (new_dir, (new_x, new_y), _) =
      case legal_dirs of
        [] -> (g.dir, snapToWall g.pos g.dir, 0)
        _  ->
          let
            dirs_and_dists =
              Lst.map
                (\dr ->
                   let
                     n_pos = updatePos g.pos dr delta
                   in
                     (dr, n_pos, distToTarg n_pos))
                legal_dirs
            fst_dir = unsafeFromJust <| Lst.head dirs_and_dists
            max_dst (dr1, ps1, dst1) (dr2, ps2, dst2) =
              if dst1 < dst2 then
                (dr1, ps1, dst1)
              else
                (dr2, ps2, dst2)
          in
            Lst.foldl max_dst fst_dir dirs_and_dists
    new_pos =
      if new_x < 0 then
        (toFloat numCols - 1, new_y)
      else if new_x > toFloat numCols - 1 then
        (0, new_y)
      else
        (new_x, new_y)
  in
    if g.mode == Inactive then
      g
    else {g | dir    = new_dir,
              pos    = new_pos,
              prvPos = if new_x < 0 then
                         (toFloat numCols, new_y)
                       else if new_x > toFloat numCols - 1 then
                         (-1, new_y)
                       else
                         g.pos}

type ScareUpdate = NoChange | MakeScary | MakeScared

swapMode : State -> ScareUpdate -> State
swapMode st upd =
  let
      new_mode =
          case st.defaultMode of
            Scatter -> Chase
            Chase -> Scatter
            _ -> st.defaultMode
      update g =
          let
              new_dir =
                  case g.dir of
                    Left  -> Right
                    Right -> Left
                    Up    -> Down
                    Down  -> Up
              new_pos = updatePos g.pos new_dir ghostPace
          in
            case g.self of
              Dead -> g
              _    ->
                {g | dir  = new_dir, pos = new_pos,
                     mode = if not <| ghostActive g then
                              g.mode
                            else
                              case upd of
                                NoChange   -> new_mode
                                MakeScary  -> st.defaultMode
                                MakeScared -> Flee,
                     self = case upd of
                               MakeScared -> Scared
                               MakeScary  -> Normal
                               _ -> g.self}
  in
    {st | blinky       = update st.blinky,
           pinky       = update st.pinky,
            inky       = update st.inky,
           clyde       = update st.clyde,
           modeChanges =
            case upd of
              NoChange -> unsafeFromJust (Lst.tail st.modeChanges)
              _        -> st.modeChanges,
         defaultMode   = new_mode}

leaveHouse : Ghost -> Int -> Bool
leaveHouse g numPells =
    case g.name of
      "inky"   -> numPells > 30
      "clyde"  -> numPells > 80
      "pinky"  -> True
      "blinky" -> True
      _ -> False

makeFlee g = {g | self = Scared, mode = Flee}

updateGhosts : State -> Bool -> State
updateGhosts st atePill =
    if st.timers.fleeTimer < fleeTime && not atePill && (Lst.isEmpty st.modeChanges || st.timers.gameTimer < (unsafeFromJust <| Lst.head st.modeChanges))
    then
      let
          b = st.blinky
          p = st.pinky
          i = st.inky
          c = st.clyde
          pac = st.pacman
          (bt, bg) = blinkyTarget b pac st.defaultMode st.pellsAte
          (pt, pg) = pinkyTarget  p pac st.defaultMode st.pellsAte
          (ct, cg) = clydeTarget  c pac st.defaultMode st.pellsAte
          (it, ig) = inkyTarget i b pac st.defaultMode st.pellsAte
      in
          {st | blinky = updateGhostPos bg bt,
                 pinky = updateGhostPos pg pt,
                  inky = updateGhostPos ig it,
                 clyde = updateGhostPos cg ct}
    else
      if st.timers.fleeTimer >= fleeTime then
        swapMode st MakeScary
      else if atePill then
        swapMode st MakeScared
      else
        swapMode st NoChange

blinkyTarget : Ghost -> Pacman -> Mode -> Int -> (Pos, Ghost)
blinkyTarget g p dMode pells =
    case g.mode of
      Scatter -> (g.target, g)
      Chase -> (p.pos, g)
      Flee ->
          let
              gen = R.pair (R.int 0 (numCols - 1)) (R.int 0 (numRows - 1))
              ((rx, ry), new_seed) = R.generate gen g.seed
          in
            (pfi (rx,ry), {g | seed = new_seed})
      Inactive -> if (leaveHouse g pells) then ((13, 11), {g | mode = House})
               else (initBlinky.pos, g)
      House ->
          if g.pos == initBlinky.pos then
            if g.self == Dead then
              (initPinky.pos, {g | mode = Center})
            else
              blinkyTarget {g | mode = dMode} p dMode pells
          else
            (initBlinky.pos, g)
      Center -> if g.pos == initPinky.pos then blinkyTarget {g | mode = House
                                                           , self = Normal} p dMode pells
                else (initPinky.pos, g)

pinkyTarget : Ghost -> Pacman -> Mode -> Int -> (Pos, Ghost)
pinkyTarget g p dMode pells =
    case g.mode of
      Scatter -> (g.target, g)
      Chase -> let (px, py) = p.pos in
               case p.dir of
                 Left -> ((px-4, py), g)
                 Right -> ((px+4, py), g)
                 Down -> ((px, py+4), g)
                 Up -> ((px, py-4), g)
      Flee ->
          let
              gen = R.pair (R.int 0 (numCols - 1)) (R.int 0 (numRows - 1))
              ((rx, ry), new_seed) = R.generate gen g.seed
          in
            (pfi (rx,ry), {g | seed = new_seed})
      Inactive -> if (leaveHouse g pells) then ((13, 11), {g | mode = House})
               else (initPinky.pos, g)
      House -> if g.pos == initBlinky.pos then
                 if g.self == Dead then
                    (initPinky.pos, {g | mode = Center})
                 else
                    pinkyTarget {g | mode = dMode} p dMode pells
               else
                 (initBlinky.pos, g)
      Center -> if g.pos == initPinky.pos then pinkyTarget {g | mode = House
                                                           , self = Normal} p dMode pells
                else (initPinky.pos, g)

inkyTarget : Ghost -> Ghost -> Pacman -> Mode -> Int -> (Pos, Ghost)
inkyTarget i b p dMode pells =
    case i.mode of
      Scatter -> (i.target, i)
      Chase ->
          let
              (px, py) = p.pos
              (x, y) = case p.dir of
                         Left -> (px-2, py)
                         Right -> (px+2, py)
                         Down -> (px, py+2)
                         Up -> (px, py-2)
              (bx, by) = b.pos
          in
            ((2 * (x-bx), 2 * (y-by)), i)
      Flee ->
          let
              gen = R.pair (R.int 0 (numCols - 1)) (R.int 0 (numRows - 1))
              ((rx, ry), new_seed) = R.generate gen i.seed
          in
            (pfi (rx,ry), {i | seed = new_seed})
      Inactive -> if (leaveHouse i pells) then ((13, 11), {i | mode = House})
               else (initInky.pos, i)
      House ->
          if i.pos == initBlinky.pos then
            if i.self == Dead then
              (initPinky.pos, {i | mode = Center})
            else
              inkyTarget {i | mode = dMode} b p dMode pells
          else
            (initBlinky.pos, i)
      Center -> if i.pos == initPinky.pos then inkyTarget {i | mode = House
                                                          , self = Normal} b p dMode pells
                else (initPinky.pos, i)

clydeTarget : Ghost -> Pacman -> Mode -> Int -> (Pos, Ghost)
clydeTarget g p dMode pells =
    case g.mode of
      Scatter -> (g.target, g)
      Chase -> if (dist g.pos p.pos) < 8 then (p.pos, g)
               else (g.target, g)
      Flee ->
          let
              gen = R.pair (R.int 0 (numCols - 1)) (R.int 0 (numRows - 1))
              ((rx, ry), new_seed) = R.generate gen g.seed
          in
            (pfi (rx, ry), {g | seed = new_seed})
      Inactive -> if  (leaveHouse g pells) then ((13, 11), {g | mode = House})
                  else (initClyde.pos, g)
      House ->
        if g.pos == initBlinky.pos then
          if g.self == Dead then
            (initPinky.pos, {g | mode = Center})
          else
            clydeTarget {g | mode = dMode} p dMode pells
        else
          (initBlinky.pos, g)
      Center ->
        if g.pos == initPinky.pos then
          clydeTarget {g | mode = House, self = Normal} p dMode pells
        else
          (initPinky.pos, g)
