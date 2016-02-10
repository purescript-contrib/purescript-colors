module Color.Gradient
  ( InterpolationMode(..)
  , linearGradient
  , interpolate
  ) where

import Prelude
import Data.Array ((..))
import Data.Foldable (minimumBy)
import Data.Int (toNumber)
import Data.Maybe.Unsafe (fromJust)
import Data.Ord (max, comparing)
import Math (abs)

import Color

-- | Defines the colorspace in which an interpolation takes place.
data InterpolationMode = HSL | RGB

-- | A range of colors that linearly interpolates between the two boundaries.
-- | The interpolation uses the defined colorspace. For the HSL colorspace, the
-- | shortest path is chosen along the circle of hue values. The total number
-- | of colors can be specified.
linearGradient :: InterpolationMode -> Int -> Color -> Color -> Array Color
linearGradient mode steps c1 c2 = do
  i <- 0 .. (steps' - 1)
  let frac = toNumber i / toNumber (steps' - 1)
  pure $ interpolate mode c1 c2 frac

  where
    steps' = max 2 steps

interpolateValue :: Number -> Number -> Number -> Number
interpolateValue fraction a b = a + fraction * (b - a)

-- | Linearly interpolate between two colors, using the specified color space.
interpolate :: InterpolationMode -> Color -> Color -> Number -> Color
interpolate HSL c1 c2 frac = hsla
    (interpolateValue frac hue.from hue.to)
    (interpolateValue frac f.s t.s)
    (interpolateValue frac f.l t.l)
    (interpolateValue frac f.a t.a)

  where
    f = toHSLA c1
    t = toHSLA c2
    paths = [ { from: f.h, to: t.h }
            , { from: f.h, to: t.h + 360.0 }
            , { from: f.h + 360.0, to: t.h }
            ]
    dist { from, to } = abs (to - from)
    hue = fromJust (minimumBy (comparing dist) paths)

interpolate RGB c1 c2 frac = rgba'
    (interpolateValue frac f.r t.r)
    (interpolateValue frac f.g t.g)
    (interpolateValue frac f.b t.b)
    (interpolateValue frac f.a t.a)

  where
    f = toRGBA' c1
    t = toRGBA' c2
