module Color.Gradient where

import Prelude
import Data.Array ((..))
import Data.Int (toNumber)
import Data.Ord (max)

import Color

-- | A range of colors that linearly interpolates between the two boundaries.
hslGradient :: Int -> Color -> Color -> Array Color
hslGradient steps from to = do
  i <- 0 .. (steps' - 1)
  let frac = toNumber i / toNumber (steps' - 1)

  return $ hsla
    (interpolate frac f.h t.h)
    (interpolate frac f.s t.s)
    (interpolate frac f.l t.l)
    (interpolate frac f.a t.a)

  where
    interpolate fraction a b = a + fraction * (b - a)
    f = toHSLA from
    t = toHSLA to
    steps' = max 2 steps
