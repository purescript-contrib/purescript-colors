-- | This module defines color scales. A color scale is a continuum of colors
-- | that is defined on the closed interval `[0, 1]`. It is defined by the two
-- | colors at each endpoint (0 and 1) and possibly additional color stops
-- | in between. If a color scale is sampled between two color stops, the
-- | specified color space is used for linear interpolation (see `mix`).
module Color.Scale
  ( ColorStop
  , ColorScale
  , colorStop
  , colorScale
  , uniformScale
  , addStop
  , sample
  , colors
  , modify
  , grayscale
  , hot
  , cool
  , spectrum
  , spectrumLCh
  , blueToRed
  , yellowToRed
  , cssColorStops

  -- more advanced staff
  , cssColorStopsSample
  , cssColorStopsRGB
  , Sampler
  , mkSimpleSampler
  , addStop'
  , uniformScale'
  , colorScale'
  , colors'
  , cubehelixSample
  , ColorScaleBuilder(..)
  ) where

import Prelude

import Color (Color, ColorSpace(..), Mixer, black, cssStringHSLA, cubehelixMix, fromInt, hsl, lch, mix, white)
import Color.Scheme.X11 (red, yellow)
import Data.Foldable (class Foldable, intercalate, foldl)
import Data.Int (toNumber)
import Data.List (List(..), insertBy, snoc, (..), fromFoldable, length, zipWith, (:))


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
data ColorScaleBuilder = ColorScaleBuilder Color (List ColorStop) Color
data ColorScale = ColorScale ColorSpace ColorScaleBuilder

-- | Create a color scale. The color space is used for interpolation between
-- | different stops. The first `Color` defines the left end (color at ratio
-- | 0.0), the list of stops defines possible intermediate steps and the second
-- | `Color` argument defines the right end point (color at ratio 1.0).
colorScale :: ColorSpace -> Color -> List ColorStop -> Color -> ColorScale
colorScale space b middle e = ColorScale space $ colorScale' b middle e

colorScale' :: Color -> List ColorStop -> Color -> ColorScaleBuilder
colorScale' = ColorScaleBuilder

-- | Create a uniform color scale from a list of colors that will be evenly
-- | spaced on the scale.
uniformScale :: forall f. Foldable f => ColorSpace -> Color -> f Color -> Color -> ColorScale
uniformScale mode b middle e = ColorScale mode $ uniformScale' b middle e

uniformScale' :: forall f. Foldable f => Color -> f Color -> Color -> ColorScaleBuilder
uniformScale' b middle e = colorScale' b stops e
  where
    cs = fromFoldable middle
    len = length cs
    n = 1 + len
    stops = zipWith makeStop (1 .. n) cs
    makeStop i col = colorStop col (toNumber i / toNumber n)

-- | Add a stop to a color scale.
addStop :: ColorScale -> Color -> Number -> ColorScale
addStop (ColorScale mode b) c r = ColorScale mode $ addStop' b c r

addStop' :: ColorScaleBuilder -> Color -> Number -> ColorScaleBuilder
addStop' (ColorScaleBuilder b middle e) c r =
  ColorScaleBuilder b (insertBy (comparing stopRatio) stop middle) e
    where stop = colorStop c r

sample :: Sampler ColorScale
sample (ColorScale mode scale) = mkSimpleSampler (mix mode) scale

type Sampler a = a -> Number -> Color

cubehelixSample :: Sampler ColorScaleBuilder
cubehelixSample = mkSimpleSampler $ cubehelixMix 1.0




mkSimpleSampler :: Mixer -> Sampler ColorScaleBuilder
-- | Get the color at a specific point on the color scale (number between 0 and
-- | 1). If the number is smaller than 0, the color at 0 is returned. If the
-- | number is larger than 1, the color at 1 is returned.
mkSimpleSampler mixer (ColorScaleBuilder b middle e) x
  | x <= 0.0 = b
  | x >= 1.0 = e
  | otherwise = go b 0.0 (middle `snoc` colorStop e 1.0)
  where
    go col _ Nil = col
    go c1 left (Cons (ColorStop c2 right) rest) =
      if between left right x
        then if left == right
               then c1
               else mixer c1 c2 ((x - left) / (right - left))
        else go c2 right rest

-- | Takes sampling function and returns A list of colors that is sampled from
-- | a color scale. The number of colors can be specified.
colors :: ColorScale -> Int -> List Color
colors scale = colors' (sample scale)

colors' :: (Number -> Color) -> Int -> List Color
colors' f 0 = Nil
colors' f num = map mkColor $ 0 .. (num - 1)
  where mkColor i = f (toNumber i / toNumber (num - 1))

-- | Modify the color scale by applying the given function to each color stop.
-- | The first argument is the position of the color stop.
modify :: (Number -> Color -> Color) -> ColorScaleBuilder -> ColorScaleBuilder
modify f (ColorScaleBuilder start middle end) =
  ColorScaleBuilder (f 0.0 start) (f' <$> middle) (f 1.0 end)
    where f' (ColorStop col r) = ColorStop (f r col) r

-- | A scale of colors from black to white.
grayscale :: ColorScale
grayscale = colorScale HSL black Nil white

-- | A spectrum of fully saturated hues (HSL color space).
spectrum :: ColorScale
spectrum = colorScale HSL end stops end
  where
    end = hsl 0.0 1.0 0.5
    stops = do
      i <- 1 .. 35
      let r = toNumber i
      pure $ colorStop (hsl (10.0 * r) 1.0 0.5) (r / 36.0)

-- | A perceptually-uniform spectrum of all hues (LCh color space).
spectrumLCh :: ColorScale
spectrumLCh = colorScale HSL end stops end
  where
    lightness = 70.0
    chroma = 35.0
    end = lch lightness chroma 0.0
    stops = do
      i <- 1 .. 35
      let r = toNumber i
      pure $ colorStop (lch lightness chroma (10.0 * r)) (r / 36.0)

-- | A perceptually-uniform, diverging color scale from blue to red, similar to
-- | the ColorBrewer scale 'RdBu'.
blueToRed :: ColorScale
blueToRed = uniformScale Lab blue (gray : Nil) red
  where
    gray = fromInt 0xf7f7f7
    red  = fromInt 0xb2182b
    blue = fromInt 0x2166ac

-- | A perceptually-uniform, multi-hue color scale from yellow to red, similar
-- | to the ColorBrewer scale YlOrRd.
yellowToRed :: ColorScale
yellowToRed = uniformScale Lab yellow (orange : Nil) red
  where
    yellow = fromInt 0xffffcc
    orange = fromInt 0xfd8d3c
    red    = fromInt 0x800026

-- | A color scale that represents 'hot' colors.
hot :: ColorScale
hot = colorScale HSL black (colorStop red 0.5 : colorStop yellow 0.75 : Nil) white

-- | A color scale that represents 'cool' colors.
cool :: ColorScale
cool = colorScale HSL (hsl 180.0 1.0 0.6) Nil (hsl 300.0 1.0 0.5)

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
cssColorStops (ColorScale RGB b) = cssColorStopsRGB b
cssColorStops (ColorScale _ scale) = cssColorStopsSample (mkSimpleSampler $ mix RGB) 10 scale

cssColorStopsSample :: Sampler ColorScaleBuilder -> Int -> ColorScaleBuilder -> String
cssColorStopsSample sampler minSamplingStepCount scale =
  cssColorStops $ ColorScale RGB csRGB
  where
  csRGB = foldl go scale additionalStops
  go scale' (ColorStop c r) = addStop' scale' c r

  additionalStops = do
    step <- 1 .. (minSamplingStepCount - 1)
    let frac = ratio (toNumber step / toNumber minSamplingStepCount)
    pure $ ColorStop (sampler scale frac) frac

cssColorStopsRGB :: ColorScaleBuilder -> String
cssColorStopsRGB (ColorScaleBuilder b Nil e) =
  cssStringHSLA b <> ", " <> cssStringHSLA e
cssColorStopsRGB (ColorScaleBuilder b middle e) =
  cssStringHSLA b <> ", "
  <> intercalate ", " (toString <$> middle)
  <> ", " <> cssStringHSLA e
  where
  toString (ColorStop c r) = cssStringHSLA c <> " " <> percentage r
  percentage r = show (r * 100.0) <> "%"
