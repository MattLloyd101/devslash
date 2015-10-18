module Main where

import Prelude

import Data.Array
import Data.Int hiding (ceil, floor, round)
import Data.Maybe
import Data.Traversable

import Control.Monad.Eff
import Control.Monad.Eff.Console hiding (error)
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Random

import Graphics.Canvas

import Math hiding (log)
import Math.Point

pointWithin :: forall eff. Rectangle -> Eff (random :: RANDOM | eff) Point
pointWithin area = do
  x <- randomRange area.x (area.x + area.w)
  y <- randomRange area.y (area.y + area.h)
  return $ { x: x, y: y }

drawCanvas :: forall eff. CanvasElement -> Array Point -> Eff (canvas :: Canvas, console :: CONSOLE | eff) (Array Context2D)
drawCanvas canvas points = do
  ctx <- getContext2D canvas
  let circles = (\p -> { x: p.x, y: p.y, r: 0.5, start: 0.0, end: 2.0 * pi }) <$> points
  setFillStyle "#000000" ctx
  traverse (\a -> fillPath ctx $ arc ctx a) circles

main = do
  canvas <- getCanvasElementById "canvas"
  case canvas of
    Nothing -> throwException $ error "There must be a canvas element called 'canvas' on the page."
    Just canvas -> do
      width <- getCanvasWidth canvas
      height <- getCanvasHeight canvas
      points <- replicateM 100 $ pointWithin { x: 0.0, y: 0.0, w: width, h: height }
      drawCanvas canvas points
