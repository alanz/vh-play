{-# LANGUAGE GeneralizedNewtypeDeriving
           , TypeFamilies
  #-}

import Data.Default
import Graphics.UI.Toy.Prelude

newtype State = State (TToy [] (CairoDraggable CairoDiagram))
  deriving (Interactive Gtk, GtkDisplay, Diagrammable Cairo)

type instance V State = R2

main :: IO ()
main = runToy (def :: State)

instance Default State where
  {-
  def = State $ TToy
      [ mkDraggable (r2 (x, y)) (circle 5 :: CairoDiagram)
      | x <- [50,60..100], y <- [50, 60..100]
      ]
  -}
  def = State $ TToy
        [
          mkDraggable (r2 (50,50)) (circle 5 :: CairoDiagram)
                      -- mkDraggable (r2 (60,50)) (line 0
        , mkDraggable (r2 (100,100)) ppp
        , mkDraggable (r2 (70,50)) (circle 5 :: CairoDiagram)
        , mkDraggable (r2 (110,110)) qqq

        ]


ppp :: CairoDiagram
-- ppp = text "F" <> square 1 # lw 0
ppp = (s darkred ||| s red) === (s pink ||| s indianred)
  where
    s c     = square 10 # fc c


qqq :: CairoDiagram
qqq = ((text' "a") ||| (text' "=") ||| (text' "1"))



text' :: String -> CairoDiagram
text' s = text s # scaleY (-1) <> square (fromIntegral (8 * length s)) # lw 0
