-- | This module provides types and functions for dealing with colors, such as
-- | the conversion between different color spaces and color manipulations.
module Color
  ( Color()
  , rgba
  , rgb
  , rgba'
  , rgb'
  , hsla
  , hsl
  , grayscale
  , toHSLA
  , toRGBA
  , cssStringHSLA
  ) where

import Prelude
import Data.Int (toNumber, round)
import Math (abs, (%))
import Data.Ord (min, max, clamp)

-- | The representation of a color.
data Color = HSLA Number Number Number Number

instance showColor :: Show Color where
  show (HSLA h s l a) = "hsla " <> show h <> " "
                                <> show s <> " "
                                <> show l <> " "
                                <> show a

-- | Equality between two `Color`s is checked up to an accuracy of 1% in the
-- | saturation, lightness and alpha channels and up to an accuracy of 1.5
-- | degree in the hue channel.
instance eqColor :: Eq Color where
  eq (HSLA h s l a) (HSLA h' s' l' a') =
    approxEqDegree h h' &&
    approxEq s s' &&
    approxEq l l' &&
    approxEq a a'
    where
      deltaDegree = 1.5
      approxEqDegree d1 d2 = diff < deltaDegree ||
                             360.0 - diff < deltaDegree
        where diff = abs (d1 - d2)

      delta = 0.01
      approxEq v1 v2 = abs (v1 - v2) < delta

-- | Create a `Color` from integer RGB values between 0 and 255 and a floating
-- | point alpha value between 0.0 and 1.0.
rgba :: Int -> Int -> Int -> Number -> Color
rgba red green blue alpha = HSLA hue saturation lightness alpha
  where
    -- RGB to HSL conversion algorithm adapted from
    -- https://en.wikipedia.org/wiki/HSL_and_HSV
    r = toNumber red   / 255.0
    g = toNumber green / 255.0
    b = toNumber blue  / 255.0

    maxChroma = max (max red green) blue
    minChroma = min (min red green) blue

    chroma = maxChroma - minChroma
    chroma' = toNumber chroma / 255.0

    hue' 0 = 0.0
    hue' c | maxChroma == red   = ((g - b) / chroma') `modPos` 6.0
           | maxChroma == green = ((b - r) / chroma') + 2.0
           | otherwise          = ((r - g) / chroma') + 4.0

    modPos x y = (x % y + y) % y

    hue = 60.0 * hue' chroma

    lightness = toNumber (maxChroma + minChroma) / (255.0 * 2.0)

    saturation | chroma == 0 = 0.0
               | otherwise = chroma' / (1.0 - abs (2.0 * lightness - 1.0))

-- | Create a `Color` from RGB values between 0 and 255.
rgb :: Int -> Int -> Int -> Color
rgb r g b = rgba r g b 1.0

-- | Create a `Color` from RGB values between 0.0 and 1.0 and an alpha value
-- | between 0.0 and 1.0.
rgba' :: Number -> Number -> Number -> Number -> Color
rgba' r g b a = rgba (round $ r * 255.0)
                     (round $ g * 255.0)
                     (round $ b * 255.0)
                     a

-- | Create a `Color` from RGB values between 0.0 and 1.0.
rgb' :: Number -> Number -> Number -> Color
rgb' r g b = rgba' r g b 1.0

-- | Create a `Color` from hue, saturation, lightness and alpha values.
hsla :: Number -> Number -> Number -> Number -> Color
hsla = HSLA

-- | Create a `Color` from hue, saturation, lightness and alpha values.
hsl :: Number -> Number -> Number -> Color
hsl h s l = HSLA h s l 1.0

-- | Create a gray tone from a lightness values (0.0 is black, 1.0 is white).
grayscale :: Number -> Color
grayscale l = hsl 0.0 0.0 l

-- | Convert a `Color` to its hue, saturation, lightness and alpha values.
toHSLA :: Color -> { h :: Number, s :: Number, l :: Number, a :: Number }
toHSLA (HSLA h s l a) = { h, s, l, a }

-- | Convert a `Color` to its red, green, blue and alpha values.
toRGBA :: Color -> { r :: Int, g :: Int, b :: Int, a :: Number }
toRGBA (HSLA h s l a) = { r, g, b, a }
  where
    r = round (255.0 * (rgb1.r + m))
    g = round (255.0 * (rgb1.g + m))
    b = round (255.0 * (rgb1.b + m))

    h'  = h / 60.0
    chr = (1.0 - abs (2.0 * l - 1.0)) * s
    m   = l - chr / 2.0
    x   = chr * (1.0 - abs (h' % 2.0 - 1.0))
    rgb1 |              h' < 1.0 = { r: chr, g: x  , b: 0.0 }
         | 1.0 <= h' && h' < 2.0 = { r: x  , g: chr, b: 0.0 }
         | 2.0 <= h' && h' < 3.0 = { r: 0.0, g: chr, b: x   }
         | 3.0 <= h' && h' < 4.0 = { r: 0.0, g: x  , b: chr }
         | 4.0 <= h' && h' < 5.0 = { r: x  , g: 0.0, b: chr }
         | otherwise             = { r: chr, g: 0.0, b: x   }

-- The CSS representation of the color in the form `hsl(..)` or `hsla(...)`.
cssStringHSLA :: Color -> String
cssStringHSLA (HSLA h s l a) =
  if a == 1.0
    then "hsl(" <> hue <> ", " <> saturation <> ", " <> lightness <> ")"
    else "hsla(" <> hue <> ", " <> saturation <> ", " <> lightness <> ", "
                 <> alpha <> ")"
  where
    hue = show (round h)
    saturation = show (round (s * 100.0)) <> "%"
    lightness = show (round (l * 100.0)) <> "%"
    alpha = show a

-- | Get the complementary color (hue rotated by 180°).
complementary :: Color -> Color
complementary (HSLA h s l a) =  HSLA h' s l a
  where h' = (h + 180.0) % 360.0

-- | Lighten a color by adding a certain amount (number between -1.0 and 1.0)
-- | to the lightness channel. If the number is negative, the color is
-- | darkened.
lighten :: Number -> Color -> Color
lighten f (HSLA h s l a) = HSLA h s l' a
  where l' = clamp 0.0 1.0 (l + f)

-- | Darken a color by subtracting a certain amount (number between -1.0 and
-- | 1.0) from the lightness channel. If the number is negative, the color is
-- | lightened.
darken :: Number -> Color -> Color
darken f = lighten (- f)
