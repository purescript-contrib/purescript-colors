module Color.Gradient where

import Prelude
import Data.Array ((..))
import Data.Foldable (minimumBy)
import Data.Int (toNumber)
import Data.Maybe.Unsafe (fromJust)
import Data.Ord (max, comparing)
import Math (abs)

import Color

-- | A range of colors that linearly interpolates between the two boundaries.
-- | The interpolation uses the HSL colorspace and always takes the shortest
-- | path along the circle of hue values.
hslGradient :: Int -> Color -> Color -> Array Color
hslGradient steps c1 c2 = do
  i <- 0 .. (steps' - 1)
  let frac = toNumber i / toNumber (steps' - 1)

  pure $ hsla
    (interpolate frac hue.from hue.to)
    (interpolate frac f.s t.s)
    (interpolate frac f.l t.l)
    (interpolate frac f.a t.a)

  where
    f = toHSLA c1
    t = toHSLA c2
    paths = [ { from: f.h, to: t.h }
            , { from: f.h, to: t.h + 360.0 }
            , { from: f.h + 360.0, to: t.h }
            ]
    dist { from, to } = abs (to - from)
    hue = fromJust (minimumBy (comparing dist) paths)
    interpolate fraction a b = a + fraction * (b - a)
    steps' = max 2 steps
