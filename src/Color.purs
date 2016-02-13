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
  , black
  , white
  , grayscale
  , toHSLA
  , toRGBA
  , toRGBA'
  , fromHexString
  , toHexString
  , cssStringHSLA
  , rotateHue
  , complementary
  , lighten
  , darken
  , saturate
  , desaturate
  , brightness
  , isLight
  , textColor
  ) where

import Prelude
import Control.Bind (join)
import Data.Array ((!!))
import Data.Int (toNumber, round)
import Data.Maybe (Maybe)
import Data.Ord (min, max, clamp)
import Data.String (length)
import Data.String.Regex (regex, parseFlags, match)
import Math (abs, (%))

-- | The representation of a color.
data Color = HSLA Number Number Number Number

instance showColor :: Show Color where
  show (HSLA h s l a) = "hsla " <> show h <> " "
                                <> show s <> " "
                                <> show l <> " "
                                <> show a

-- | Equality between two `Color`s is checked by comparing the (integer) RGB
-- | values.
instance eqColor :: Eq Color where
  eq c1 c2 = rgb1.r == rgb2.r && rgb1.g == rgb2.g &&
             rgb1.b == rgb2.b && rgb1.a == rgb2.a
    where
      rgb1 = toRGBA c1
      rgb2 = toRGBA c2

-- | Like `%`, but always positive.
modPos :: Number -> Number -> Number
modPos x y = (x % y + y) % y

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

-- | Create a `Color` from hue, saturation, lightness and alpha values. The
-- | hue is given in degrees, as a `Number` between 0.0 and 360.0. Saturation,
-- | lightness and alpha are numbers between 0.0 and 1.0.
hsla :: Number -> Number -> Number -> Number -> Color
hsla h s l a = HSLA h' s' l' a'
  where h' = h `modPos` 360.0
        s' = clamp 0.0 1.0 s
        l' = clamp 0.0 1.0 l
        a' = clamp 0.0 1.0 a

-- | Create a `Color` from hue, saturation and lightness values. The hue is
-- | given in degrees, as a `Number` between 0.0 and 360.0. Both saturation and
-- | lightness are numbers between 0.0 and 1.0.
hsl :: Number -> Number -> Number -> Color
hsl h s l = hsla h s l 1.0

-- | The color black.
black :: Color
black = hsl 0.0 0.0 0.0

-- | The color white.
white :: Color
white = hsl 0.0 0.0 1.0

-- | Create a gray tone from a lightness values (0.0 is black, 1.0 is white).
grayscale :: Number -> Color
grayscale l = hsl 0.0 0.0 l

-- | Convert a `Color` to its hue, saturation, lightness and alpha values.
toHSLA :: Color -> { h :: Number, s :: Number, l :: Number, a :: Number }
toHSLA (HSLA h s l a) = { h, s, l, a }

-- | Convert a `Color` to its red, green, blue and alpha values. The RGB values
-- | are integers in the range from 0 to 255.
toRGBA :: Color -> { r :: Int, g :: Int, b :: Int, a :: Number }
toRGBA col@(HSLA _ _ _ a) = { r, g, b, a }
  where
    c = toRGBA' col
    r = round (255.0 * c.r)
    g = round (255.0 * c.g)
    b = round (255.0 * c.b)

-- | Convert a `Color` to its red, green, blue and alpha values. All values
-- | are numbers in the range from 0.0 to 1.0.
toRGBA' :: Color -> { r :: Number, g :: Number, b :: Number, a :: Number }
toRGBA' (HSLA h s l a) = { r: col.r + m, g: col.g + m, b: col.b + m, a }
  where
    h'  = h / 60.0
    chr = (1.0 - abs (2.0 * l - 1.0)) * s
    m   = l - chr / 2.0
    x   = chr * (1.0 - abs (h' % 2.0 - 1.0))
    col |              h' < 1.0 = { r: chr, g: x  , b: 0.0 }
        | 1.0 <= h' && h' < 2.0 = { r: x  , g: chr, b: 0.0 }
        | 2.0 <= h' && h' < 3.0 = { r: 0.0, g: chr, b: x   }
        | 3.0 <= h' && h' < 4.0 = { r: 0.0, g: x  , b: chr }
        | 4.0 <= h' && h' < 5.0 = { r: x  , g: 0.0, b: chr }
        | otherwise             = { r: chr, g: 0.0, b: x   }

foreign import parseHex :: String -> Int

-- | Parse a hexadecimal RGB code of the form `#rgb` or `#rrggbb`, where the
-- | hexadecimal digits are of the format [0-9a-f] (case insensitive). Returns
-- | `Nothing` if the string is in a wrong format.
fromHexString :: String -> Maybe Color
fromHexString str = do
  groups <- match pattern str
  r <- parseHex <$> join (groups !! 1)
  g <- parseHex <$> join (groups !! 2)
  b <- parseHex <$> join (groups !! 3)
  if isShort
    then
      pure $ rgb (16 * r + r) (16 * g + g) (16 * b + b)
    else
      pure (rgb r g b)
  where
    isShort = length str == 4
    digit = "[0-9a-f]"
    single = "(" <> digit <> ")"
    pair = "(" <> digit <> digit <> ")"
    variant = if isShort
                then single <> single <> single
                else pair <> pair <> pair
    pattern = regex ("^#(?:" <> variant <> ")$") (parseFlags "i")

foreign import toHex :: Int -> String

-- | Return a hexadecimal representation of the color in the form `#rrggbb`,
-- | where `rr`, `gg` and `bb` refer to hexadecimal digits corresponding to
-- | the RGB channel values between `00` and `ff`. The alpha channel is not
-- | represented.
toHexString :: Color -> String
toHexString color = "#" <> toHex c.r <> toHex c.g <> toHex c.b
  where c = toRGBA color

-- | The CSS representation of the color in the form `hsl(..)` or `hsla(...)`.
cssStringHSLA :: Color -> String
cssStringHSLA (HSLA h s l a) =
  if a == 1.0
    then "hsl(" <> hue <> ", " <> saturation <> ", " <> lightness <> ")"
    else "hsla(" <> hue <> ", " <> saturation <> ", " <> lightness <> ", "
                 <> alpha <> ")"
  where
    hue = toString h
    saturation = toString (s * 100.0) <> "%"
    lightness = toString (l * 100.0) <> "%"
    alpha = show a
    toString n = show $ toNumber (round (100.0 * n)) / 100.0

-- | Rotate the hue of a `Color` by a certain angle.
rotateHue :: Number -> Color -> Color
rotateHue angle (HSLA h s l a) =  hsla (h + angle) s l a

-- | Get the complementary color (hue rotated by 180°).
complementary :: Color -> Color
complementary = rotateHue 180.0

-- | Lighten a color by adding a certain amount (number between -1.0 and 1.0)
-- | to the lightness channel. If the number is negative, the color is
-- | darkened.
lighten :: Number -> Color -> Color
lighten f (HSLA h s l a) = hsla h s (l + f) a

-- | Darken a color by subtracting a certain amount (number between -1.0 and
-- | 1.0) from the lightness channel. If the number is negative, the color is
-- | lightened.
darken :: Number -> Color -> Color
darken f = lighten (- f)

-- | Increase the saturation of a color by adding a certain amount (number
-- | between -1.0 and 1.0) to the saturation channel. If the number is
-- | negative, the color is desaturated.
saturate :: Number -> Color -> Color
saturate f (HSLA h s l a) = hsla h (s + f) l a

-- | Decrease the saturation of a color by subtracting a certain amount (number
-- | between -1.0 and 1.0) from the saturation channel. If the number is
-- | negative, the color is saturated.
desaturate :: Number -> Color -> Color
desaturate f = saturate (- f)

-- | The percieved brightness of the color (A number between 0.0 and 1.0).
-- | See: https://www.w3.org/TR/AERT#color-contrast
brightness :: Color -> Number
brightness col = (299.0 * c.r + 587.0 * c.g + 114.0 * c.b) / 1000.0
  where c = toRGBA' col

-- | Determine whether a color is perceived as a light color.
isLight :: Color -> Boolean
isLight c = brightness c > 0.5

-- | Return a readable foreground text color (either `black` or `white`) for a
-- | given background color.
textColor :: Color -> Color
textColor c | isLight c = black
            | otherwise = white