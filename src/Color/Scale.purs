-- | This module defines color scales. A color scale is a continuum of colors
-- | that is defined on the closed interval `[0, 1]`. It is defined by the two
-- | colors at each endpoint (0 and 1) and possibly additional color stops
-- | in between. If a color scale is sampled between two color stops, the
-- | specified color space is used for linear interpolation (see `mix`).
module Color.Scale
  ( ColorStop()
  , colorStop
  , ColorScale()
  , colorScale
  , uniformScale
  , addStop
  , sample
  , colors
  , grayscale
  , spectrum
  , cssColorStops
  ) where

import Prelude

import Data.Foldable (class Foldable, intercalate, foldl)
import Data.Int (toNumber)
import Data.List (List(..), insertBy, snoc, (..), fromFoldable, length,
                  zipWith, singleton)
import Data.Ord (comparing, clamp, between)

import Color (Color, ColorSpace(..), mix, cssStringHSLA, black, white, hsl)

-- | Ensure that a number lies in the interval [0, 1].
ratio :: Number -> Number
ratio = clamp 0.0 1.0

-- | A point on the color scale.
data ColorStop = ColorStop Color Number

-- | Create a color stop from a given `Color` and a number between 0 and 1.
-- | If the number is outside this range, it is clamped.
colorStop :: Color -> Number -> ColorStop
colorStop c r = ColorStop c (ratio r)

stopRatio :: ColorStop -> Number
stopRatio (ColorStop _ r) = r

stopColor :: ColorStop -> Color
stopColor (ColorStop c _) = c

-- | A color scale.
data ColorScale = ColorScale ColorSpace Color (List ColorStop) Color

-- | Create a color scale. The color space is used for interpolation between
-- | different stops. The first `Color` defines the left end (color at ratio
-- | 0.0), the list of stops defines possible intermediate steps and the second
-- | `Color` argument defines the right end point (color at ratio 1.0).
colorScale :: ColorSpace -> Color -> List ColorStop -> Color -> ColorScale
colorScale = ColorScale

-- | Create a uniform color scale from a list of colors that will be evenly
-- | spaced on the scale.
uniformScale :: forall f. Foldable f => ColorSpace -> Color -> f Color -> Color -> ColorScale
uniformScale mode b middle e = colorScale mode b stops e
  where
    cs = fromFoldable middle
    len = length cs
    n = 2 + len
    stops = zipWith makeStop (1 .. (1 + len)) cs
    makeStop i col = colorStop col (toNumber i / toNumber n)

-- | Add a stop to a color scale.
addStop :: ColorScale -> Color -> Number -> ColorScale
addStop (ColorScale mode b middle e) c r =
  ColorScale mode b (insertBy (comparing stopRatio) stop middle) e
    where stop = colorStop c r

-- | Get the color at a specific point on the color scale (number between 0 and
-- | 1). If the number is smaller than 0, the color at 0 is returned. If the
-- | number is larger than 1, the color at 1 is returned.
sample :: ColorScale -> Number -> Color
sample scale@(ColorScale mode b middle e) x
  | x < 0.0 = b
  | x > 1.0 = e
  | otherwise = go b 0.0 (middle `snoc` colorStop e 1.0)
  where
    go col _ Nil = col
    go c1 left (Cons (ColorStop c2 right) rest) =
      if between left right x
        then if left == right
               then c1
               else mix mode c1 c2 ((x - left) / (right - left))
        else go c2 right rest

-- | A list of colors that is sampled from a color scale. The number of colors
-- | can be specified.
colors :: ColorScale -> Int -> List Color
colors (ColorScale _ _ _ _) 0 = Nil
colors (ColorScale _ b _ _) 1 = singleton b
colors scale num = do
  i <- 0 .. (num - 1)
  pure $ sample scale (toNumber i / toNumber (num - 1))

-- | A scale of colors from black to white.
grayscale :: ColorScale
grayscale = colorScale RGB black Nil white

-- | A spectrum of all hues (fully saturated).
spectrum :: ColorScale
spectrum = colorScale HSL end stops end
  where
    end = hsl 0.0 1.0 0.5
    stops = do
      i <- 1 .. 35
      let r = toNumber i
      return $ colorStop (hsl (10.0 * r) 1.0 0.5) (r / 36.0)

-- | A CSS representation of the color scale in the form of a comma-separated
-- | list of color stops. This list can be used in a `linear-gradient` or
-- | a similar construct.
-- |
-- | Note that CSS uses the RGB space for color interpolation. Consequently, if
-- | the color scale is in RGB mode, this is just a list of all defined color
-- | stops.
-- |
-- | For other color spaces, the color scale is sampled at (at least) 10
-- | different points. This should give a reasonable approximation to the true
-- | gradient in the specified color space.
cssColorStops :: ColorScale -> String
cssColorStops (ColorScale RGB b Nil e) =
       cssStringHSLA b <> ", " <> cssStringHSLA e
cssColorStops (ColorScale RGB b middle e) =
       cssStringHSLA b <> ", "
    <> intercalate ", " (toString <$> middle)
    <> ", " <> cssStringHSLA e
  where
    toString (ColorStop c r) = cssStringHSLA c <> " " <> percentage r
    percentage r = show (r * 100.0) <> "%"

cssColorStops scale@(ColorScale HSL b middle e) = cssColorStops csRGB
  where
    csRGB' = ColorScale RGB b middle e

    csRGB = foldl addStop' csRGB' additionalStops
    addStop' scale (ColorStop c r) = addStop scale c r

    additionalStops = do
      step <- 1 .. 9
      let frac = ratio (toNumber step / 10.0)
      return $ ColorStop (sample scale frac) frac
