module Main where

import Prelude

import Data.Array
import Data.Int hiding (ceil, floor, round)
import Data.Maybe
import Data.Traversable

import DOM.RequestAnimationFrame
import DOM (DOM())

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

drawCircles :: forall eff. Context2D -> Array Arc -> Eff (canvas :: Canvas | eff) (Array Context2D)
drawCircles ctx circles = do
  setFillStyle "#000000" ctx
  traverse (\a -> fillPath ctx $ arc ctx a) circles

update :: forall eff. Rectangle -> Eff (random :: RANDOM | eff) (Array Arc)
update { w: width, h: height } = do
  points <- replicateM 100 $ pointWithin { x: 0.0, y: 0.0, w: width, h: height }
  return $ (\p -> { x: p.x, y: p.y, r: 0.5, start: 0.0, end: 2.0 * pi }) <$> points

render :: forall eff. CanvasElement -> Rectangle -> Array Arc -> Eff (canvas :: Canvas | eff) (Array Context2D)
render canvas canvasDims circles = do
  ctx <- getContext2D canvas
  clearRect ctx canvasDims
  drawCircles ctx circles

updateLoop :: forall eff. CanvasElement -> Eff (canvas :: Canvas, random :: RANDOM, dom :: DOM | eff) Unit
updateLoop canvas = do
  width <- getCanvasWidth canvas
  height <- getCanvasHeight canvas
  let canvasDims = { x: 0.0, y: 0.0, w: width, h: height }
  circles <- update canvasDims
  render canvas canvasDims circles
  requestAnimationFrame $ updateLoop canvas

main = do
  canvas <- getCanvasElementById "canvas"
  case canvas of
    Nothing -> throwException $ error "There must be a canvas element called 'canvas' on the page."
    Just canvas -> requestAnimationFrame $ updateLoop canvas
