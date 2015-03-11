module PacModels where
import Graphics.Collage (..)
import Graphics.Element (Element, size)
import Graphics.Element as El
import Color (..)
import Signal (Signal)
import Signal as Sig
import Time as Tm
import Window as Win
import List

-- direction hack to avoid circular dependency
type ModDir = Left | Right | Up | Down

-- Colors
pink = rgb 255 186 210
mistyrose = rgb 238 213 210

background : Float -> Float -> Form
background w h =
  filled black <| rect w h

wall : Float -> Form
wall r =
    filled blue <| square r

emptySpace : Float -> Form
emptySpace r =
    filled black <| square r

gate r =
    group [square r |> filled black
          , rect r (r / 2) |> filled pink]

pacman : ModDir -> Float -> Bool -> Form
pacman d r ravi =
    let
        m = r / (sqrt 2)
        r' = r * 1.03
        tri = case d of
                Left -> polygon [(0,0), (-r',-m), (-r',m)]
                Right -> polygon [(0,0), (r',-m), (r',m)]
                Up  -> polygon [(0,0), (m,r'), (-m,r')]
                Down -> polygon [(0,0), (m,-r'), (-m,-r')]
        s = 2 * (floor r)
    in
      if ravi then group [toForm <| El.fittedImage s s "/ravi.png"
                    , filled black <| tri]
      else group [filled yellow <| circle r, filled black <| tri]

makeP c r t = group [filled c <| circle r, t]
makeR r t = group [toForm <| El.fittedImage (2* (floor r)) (2 * (floor r)) "/ravi.png", t]

animatePacman : ModDir -> Float -> Bool -> List Form
animatePacman d r ravi =
    let
        radsToPts rad = (r * (cos rad), r * (sin rad))
        ps = List.map radsToPts [pi/4,(5* pi)/16, (6 * pi)/ 16, (7 * pi)/ 16]

        makeTri (x,y) =
            group <| List.map (filled black) [polygon [(x,y), (r,r), (r,-r),(x,-y)]
                                             , polygon [(0,0), (x,y), (x,-y)]]

        makeOther (x,y) =
            group <| List.map (filled black) [polygon [(0,r),(r,r),(r,-r),(0,-r)]
                                             , polygon [(0,0),(-x,y),(0,r)]
                                             , polygon [(0,0), (-x,-y),(0,-r)]
                                             , polygon [(-x,y), (0,y), (0,r), (-x,r)]
                                             , polygon [(-x,-y), (0,-y), (0,-r), (-x,-r)]]
        other = (List.reverse ps) ++ (List.map radsToPts [(3 * pi)/16, (2 *pi)/16, pi/16, pi/32, pi/64])

        raviCase = List.map (makeR r)
                   ((List.map makeTri ps)
                    ++ [filled black <| polygon [(0,r),(r,r),(r,-r),(0,-r)]]
                           ++   (List.map makeOther other))
        normalCase =  List.map (makeP yellow r)
                      ((List.map makeTri ps)
                       ++ [filled black <| polygon [(0,r),(r,r),(r,-r),(0,-r)]]
                              ++   (List.map makeOther other))
        rightCase = if ravi then raviCase
                    else normalCase

    in
      case d of
        Right -> rightCase
        Left -> List.map (rotate pi) rightCase
        Up -> List.map (rotate (pi / 2)) rightCase
        Down -> List.map (rotate ((3 * pi) / 2)) rightCase


-- If the ghost is in scared mode, g = 'scared'
ghost : String -> Float -> Float -> Bool -> Form
ghost g w h ravi =
    if | (g /= "scared") && ravi && (g /= "dead") -> toForm <| El.fittedImage (floor w) (floor h) ("/" ++ g ++ "ravi.png")
       | otherwise ->
           toForm <| El.fittedImage (floor w) (floor h) ("/" ++ g ++ ".png")

pellet : Float -> Form
pellet r =
  filled white <| circle r

pill : Float -> Form
pill r =
  filled lightOrange <| circle r

fruit : Form
fruit =
  filled red <| ngon 3 1

sz_toForm : Float -> Float -> Form -> Form
sz_toForm w h e =
  scale w e

cherry : Float -> Float -> Form
cherry w h =
  sz_toForm w h fruit

strawberry : Float -> Float -> Form
strawberry w h =
  sz_toForm w h fruit

orange : Float -> Float -> Form
orange w h =
  sz_toForm w h fruit

apple : Float -> Float -> Form
apple w h =
  sz_toForm w h fruit

melon : Float -> Float -> Form
melon w h =
  sz_toForm w h fruit

galaxian : Float -> Float -> Form
galaxian w h =
  sz_toForm w h fruit

bell : Float -> Float -> Form
bell w h =
  sz_toForm w h fruit

key : Float -> Float -> Form
key w h =
  sz_toForm w h fruit

view : (Int, Int) -> List Form -> Element
view (w, h) (f::_) =
  collage w h [background (toFloat w) (toFloat h), f]

-- --upstate : a -> List Form -> List Form
-- upstate _ (f::fs) =
--  fs ++ [f]
-- main =
--     Sig.map2 view Win.dimensions (Sig.foldp upstate
--                                          (animatePacman Left 50) (Tm.every (Tm.second / 20)))
