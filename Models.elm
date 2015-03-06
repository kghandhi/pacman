module Models where

import Graphics.Collage (..)
import Graphics.Element (Element, size)
import Color (..)
import Signal (Signal)
import Signal as Sig
import Time as Tm
import Window as Win

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

pacman : ModDir -> Float -> Form
pacman d r =
    let
        m = r / (sqrt 2)
        tri = case d of
                Left -> polygon [(0,0), (-r,-m), (-r,m)]
                Right -> polygon [(0,0), (r,-m), (r,m)]
                Up  -> polygon [(0,0), (m,r), (-m,r)]
                Down -> polygon [(0,0), (m,-r), (-m,-r)]
    in
      group [filled yellow <| circle r, filled black <| tri]


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

upstate : a -> List Form -> List Form
upstate _ (f::fs) =
  fs ++ [f]

main : Signal Element
main =
  -- Sig.map2 view Win.dimensions (Sig.foldp upstate [(pacman 25),(pellet 10),(pill 25),(cherry 30 40)] (Tm.every Tm.second))

    Sig.map2 view Win.dimensions (Sig.foldp upstate
                                         [(pacman Left 25),
                                          (pacman Right 25),
                                          (pacman Up 25),
                                          (pacman Down 25)] (Tm.every Tm.second))


view : (Int, Int) -> List Form -> Element
view (w, h) (f::_) =
  collage w h [background (toFloat w) (toFloat h), f]