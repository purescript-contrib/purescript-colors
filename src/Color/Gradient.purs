module Color.Gradient
  ( linearGradient
  ) where

import Prelude
import Data.Array ((..))
import Data.Int (toNumber)
import Data.Ord (max)

import Color (Color, ColorSpace, mix)

-- | A range of colors that linearly interpolates between the two boundaries.
-- | The interpolation uses the defined colorspace.
linearGradient :: ColorSpace -> Int -> Color -> Color -> Array Color
linearGradient mode steps c1 c2 = do
  i <- 0 .. (steps' - 1)
  let frac = toNumber i / toNumber (steps' - 1)
  pure $ mix mode c1 c2 frac

  where
    steps' = max 2 steps
