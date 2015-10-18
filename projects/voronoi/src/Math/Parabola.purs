module Math.Parabola where

import Prelude

import Math
import Math.Point
import Graphics.Canvas

type Parabola = { focus :: Point, directrix :: Number }

-- | Creates a Bezier Curve from a Focus and a Directrix.
-- | Directrix here is assumed to be parallel to the Y axis.
parabolaToBezier :: Parabola -> BezierCurve
parabolaToBezier { focus: { x: x, y:y }, directrix: d } = do
  let a = sqrt $ (d * d) - (y * y)
  let px = x - a
  let py = 0.0
  let cp1x = x
  let cp1y = y + d
  let cp2x = x
  let cp2y = 0.0
  { x: px, y: py, cp1x: cp1x, cp1y: cp1y, cp2x: cp2x, cp2y: cp2y }
