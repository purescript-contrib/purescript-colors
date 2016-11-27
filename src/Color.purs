-- | This module provides basic types and functions for dealing with colors.
-- |
-- | Colors can be constructed from HSL values, RGB values or Hex strings /
-- | integers. In addition, a lot of standardized named colors can be found in
-- | `Color.Scheme.X11`.
-- |
-- | This module also provides functions to modify colors (e.g. lighten/darken,
-- | saturate/desaturate, complementary), to combine colors (mix) and to
-- | analyze colors (e.g. brightness, luminance, contrast).
-- |
-- | Implementation detail: Colors are represented by their HSL values (hue,
-- | saturation, lightness) internally, as this provides more flexibility than
-- | storing RGB values. In particular, note that only colors within the sRGB
-- | gamut can be represented.

module Color
  ( Color
  , ColorSpace(..)
  -- Construct
  , rgba
  , rgb
  , rgba'
  , rgb'
  , hsla
  , hsl
  , xyz
  , lab
  , lch
  , fromHexString
  , fromInt
  -- Convert
  , toHSLA
  , toRGBA
  , toRGBA'
  , toXYZ
  , toLab
  , toLCh
  , toHexString
  , cssStringHSLA
  , cssStringRGBA
  -- Basic
  , black
  , white
  , graytone
  -- Modify
  , rotateHue
  , complementary
  , lighten
  , darken
  , saturate
  , desaturate
  , toGray
  -- Combine
  , mix
  -- Analyze
  , brightness
  , luminance
  , contrast
  , isLight
  , isReadable
  , textColor
  , distance
  ) where

import Prelude
import Data.Array ((!!))
import Data.Either (either)
import Data.Foldable (minimumBy)
import Data.Int (toNumber, round, fromStringAs, toStringAs, hexadecimal)
import Data.Int.Bits ((.&.), shr)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.String (length)
import Data.String.Regex (regex, parseFlags, match)
import Math (abs, (%), pow, cos, sin, pi, sqrt, atan2)
import Partial.Unsafe (unsafePartial)

-- | The representation of a color.
-- |
-- | Note:
-- | - The `Eq` instance compares two `Color`s by comparing their (integer) RGB
-- |   values. This is different from comparing the HSL values (for example,
-- |   HSL has many different representations of black (arbitrary hue and
-- |   saturation values).
-- | - Colors outside the sRGB gamut which cannot be displayed on a typical
-- |   computer screen can not be represented by `Color`.
-- |
data Color = HSLA Number Number Number Number

-- | Definition of a color space.
-- |
-- | * `RGB`: red, green, blue
-- | * `HSL`: hue, saturation, lightness
-- | * `LCh`: Lightness, chroma, hue
-- | * `Lab`: Lightness, a, b
data ColorSpace = RGB | HSL | LCh | Lab

instance showColor :: Show Color where
  show c = "rgba " <> show col.r <> " "
                   <> show col.g <> " "
                   <> show col.b <> " "
                   <> show col.a
    where col = toRGBA c

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
rgba red' green' blue' alpha = HSLA hue saturation lightness alpha
  where
    -- RGB to HSL conversion algorithm adapted from
    -- https://en.wikipedia.org/wiki/HSL_and_HSV
    red = clamp 0 255 red'
    blue = clamp 0 255 blue'
    green = clamp 0 255 green'

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

-- | Create a `Color` from integer RGB values between 0 and 255.
rgb :: Int -> Int -> Int -> Color
rgb r g b = rgba r g b 1.0

-- | Create a `Color` from RGB and alpha values between 0.0 and 1.0.
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

-- | Create a `Color` from XYZ coordinates in the CIE 1931 color space. Note
-- | that a `Color` always represents a color in the sRGB gamut (colors that
-- | can be represented on a typical computer screen) while the XYZ color space
-- | is bigger. This function will tend to create fully saturated colors at the
-- | edge of the sRGB gamut if the coordinates lie outside the sRGB range.
-- |
-- | See:
-- | - https://en.wikipedia.org/wiki/CIE_1931_color_space
-- | - https://en.wikipedia.org/wiki/SRGB
xyz :: Number -> Number -> Number -> Color
xyz x y z = rgb' r g b
  where
    r = f ( 3.2406 * x - 1.5372 * y - 0.4986 * z)
    g = f (-0.9689 * x + 1.8758 * y + 0.0415 * z)
    b = f ( 0.0557 * x - 0.2040 * y + 1.0570 * z)

    f c | c <= 0.0031308 = 12.92 * c
        | otherwise      = 1.055 * (c `pow` (1.0 / 2.4)) - 0.055

-- Illuminant D65 constants used for Lab color space conversions.
d65 :: { xn :: Number, yn :: Number, zn :: Number }
d65 =
  { xn: 0.950470
  , yn: 1.0
  , zn: 1.088830
}

-- | Create a `Color` from L, a and b coordinates coordinates in the Lab color
-- | space.
-- | Note: See documentation for `xyz`. The same restrictions apply here.
-- |
-- | See: https://en.wikipedia.org/wiki/Lab_color_space
lab :: Number -> Number -> Number -> Color
lab l a b = xyz x y z
  where
    l' = (l + 16.0) / 116.0
    x = d65.xn * finv (l' + a / 500.0)
    y = d65.yn * finv l'
    z = d65.zn * finv (l' - b / 200.0)

    delta = 6.0 / 29.0
    finv t | t > delta   = t `pow` 3.0
           | otherwise = 3.0 * delta * delta * (t - 4.0 / 29.0)

-- | Create a `Color` from lightness, chroma and hue coordinates in the CIE LCh
-- | color space. This is a cylindrical transform of the Lab color space.
-- | Note: See documentation for `xyz`. The same restrictions apply here.
-- |
-- | See: https://en.wikipedia.org/wiki/Lab_color_space
lch :: Number -> Number -> Number -> Color
lch l c h = lab l a b
  where
    deg2rad = pi / 180.0
    a = c * cos (h * deg2rad)
    b = c * sin (h * deg2rad)

-- | Parse a hexadecimal RGB code of the form `#rgb` or `#rrggbb`. The `#`
-- | character is required. Each hexadecimal digit is of the form `[0-9a-fA-F]`
-- | (case insensitive). Returns `Nothing` if the string is in a wrong format.
fromHexString :: String -> Maybe Color
fromHexString str = do
  pattern <- hush mPattern
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
    mPattern = regex ("^#(?:" <> variant <> ")$") (parseFlags "i")
    hush = either (const Nothing) Just
    parseHex = fromMaybe 0 <<< fromStringAs hexadecimal

-- | Converts an integer to a color (RGB representation). `0` is black and
-- | `0xffffff` is white. Values outside this range will be clamped.
-- |
-- | This function is useful if you want to hard-code Hex values. For example:
-- | ``` purs
-- | red = fromInt 0xff0000
-- | ```
fromInt :: Int -> Color
fromInt m = rgb r g b
  where b = n .&. 0xff
        g = (n `shr` 8) .&. 0xff
        r = (n `shr` 16) .&. 0xff
        n = clamp 0 0xffffff m

-- | Convert a `Color` to its hue, saturation, lightness and alpha values. See
-- | `hsla` for the ranges of each channel.
toHSLA :: Color -> { h :: Number, s :: Number, l :: Number, a :: Number }
toHSLA (HSLA h s l a) = { h, s, l, a }

-- | Convert a `Color` to its red, green, blue and alpha values. The RGB values
-- | are integers in the range from 0 to 255. The alpha channel is a number
-- | between 0.0 and 1.0.
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

-- | Get XYZ coordinates according to the CIE 1931 color space.
-- |
-- | See:
-- | - https://en.wikipedia.org/wiki/CIE_1931_color_space
-- | - https://en.wikipedia.org/wiki/SRGB
toXYZ :: Color -> { x :: Number, y :: Number, z :: Number }
toXYZ c = { x, y, z }
  where
    x = 0.4124 * r + 0.3576 * g + 0.1805 * b
    y = 0.2126 * r + 0.7152 * g + 0.0722 * b
    z = 0.0193 * r + 0.1192 * g + 0.9505 * b

    rec = toRGBA' c
    r = finv rec.r
    g = finv rec.g
    b = finv rec.b

    finv c' | c' <= 0.04045 = c' / 12.92
            | otherwise     = ((c' + 0.055) / 1.055) `pow` 2.4

-- | Get L, a and b coordinates according to the Lab color space.
-- |
-- | See: https://en.wikipedia.org/wiki/Lab_color_space
toLab :: Color -> { l :: Number, a :: Number, b :: Number }
toLab col = { l, a, b }
  where
    rec = toXYZ col

    fy = f (rec.y / d65.yn)

    l = 116.0 * fy - 16.0
    a = 500.0 * (f (rec.x / d65.xn) - fy)
    b = 200.0 * (fy - f (rec.z / d65.zn))

    cut = (6.0 / 29.0) `pow` 3.0
    f t | t > cut   = t `pow` (1.0 / 3.0)
        | otherwise = (1.0 / 3.0) * (29.0 / 6.0) `pow` 2.0 * t + 4.0 / 29.0

-- | Get L, C and h coordinates according to the CIE LCh color space.
-- |
-- | See: https://en.wikipedia.org/wiki/Lab_color_space
toLCh :: Color -> { l :: Number, c :: Number, h :: Number }
toLCh col = { l, c, h }
  where
    rec = toLab col

    l = rec.l
    a = rec.a
    b = rec.b

    rad2deg = 180.0 / pi

    c = sqrt (a * a + b * b)
    h = (atan2 b a * rad2deg) `modPos` 360.0

-- | Return a hexadecimal representation of the color in the form `#rrggbb`,
-- | where `rr`, `gg` and `bb` refer to hexadecimal digits corresponding to
-- | the RGB channel values between `00` and `ff`. The alpha channel is not
-- | represented.
toHexString :: Color -> String
toHexString color = "#" <> toHex c.r <> toHex c.g <> toHex c.b
  where c = toRGBA color
        toHex num = let repr = toStringAs hexadecimal num
                    in if length repr == 1
                         then "0" <> repr
                         else repr

-- | A CSS representation of the color in the form `hsl(..)` or `hsla(...)`.
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

-- | A CSS representation of the color in the form `rgb(..)` or `rgba(...)`.
cssStringRGBA :: Color -> String
cssStringRGBA col =
  if c.a == 1.0
    then "rgb(" <> red <> ", " <> green <> ", " <> blue <> ")"
    else "rgba(" <> red <> ", " <> green <> ", " <> blue <> ", "
                 <> alpha <> ")"
  where
    c = toRGBA col
    red = show c.r
    green = show c.g
    blue = show c.b
    alpha = show c.a

-- | Pure black.
black :: Color
black = hsl 0.0 0.0 0.0

-- | Pure white.
white :: Color
white = hsl 0.0 0.0 1.0

-- | Create a gray tone from a lightness values (0.0 is black, 1.0 is white).
graytone :: Number -> Color
graytone l = hsl 0.0 0.0 l

-- | Rotate the hue of a `Color` by a certain angle (in degrees).
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

-- | Convert a color to a gray tone with the same perceived luminance (see
-- | `luminance`).
toGray :: Color -> Color
toGray col = desaturate 1.0 (lch res.l 0.0 0.0)
             -- the desaturation step is only needed to correct minor rounding
             -- errors.
  where res = toLCh col

-- | Linearly interpolate between two values.
interpolate :: Number -> Number -> Number -> Number
interpolate fraction a b = a + fraction * (b - a)

-- | Linearly interpolate between two angles. Always take the shortest path
-- | along the circle.
interpolateAngle :: Number -> Number -> Number -> Number
interpolateAngle fraction a b = interpolate fraction shortest.from shortest.to
  where
    paths = [ { from: a, to: b }
            , { from: a, to: b + 360.0 }
            , { from: a + 360.0, to: b }
            ]
    dist { from, to } = abs (to - from)
    shortest = unsafePartial (fromJust (minimumBy (comparing dist) paths))

-- | Mix two colors by linearly interpolating between them in the specified
-- | color space. For the HSL colorspace, the shortest path is chosen along the
-- | circle of hue values.
mix :: ColorSpace -> Color -> Color -> Number -> Color
mix HSL c1 c2 frac = hsla
    (interpolateAngle frac f.h t.h)
    (interpolate frac f.s t.s)
    (interpolate frac f.l t.l)
    (interpolate frac f.a t.a)
  where
    f = toHSLA c1
    t = toHSLA c2

mix RGB c1 c2 frac = rgba'
    (interpolate frac f.r t.r)
    (interpolate frac f.g t.g)
    (interpolate frac f.b t.b)
    (interpolate frac f.a t.a)
  where
    f = toRGBA' c1
    t = toRGBA' c2

mix LCh c1 c2 frac = lch
    (interpolate frac f.l t.l)
    (interpolate frac f.c t.c)
    (interpolateAngle frac f.h t.h)
  where
    f = toLCh c1
    t = toLCh c2

mix Lab c1 c2 frac = lab
    (interpolate frac f.l t.l)
    (interpolate frac f.a t.a)
    (interpolate frac f.b t.b)
  where
    f = toLab c1
    t = toLab c2

-- | The percieved brightness of the color (A number between 0.0 and 1.0).
-- |
-- | See: https://www.w3.org/TR/AERT#color-contrast
brightness :: Color -> Number
brightness col = (299.0 * c.r + 587.0 * c.g + 114.0 * c.b) / 1000.0
  where c = toRGBA' col

-- | The relative brightness of a color (normalized to 0.0 for darkest black
-- | and 1.0 for lightest white), according to the WCAG definition.
-- |
-- | See: https://www.w3.org/TR/2008/REC-WCAG20-20081211/#relativeluminancedef
luminance :: Color -> Number
luminance col = 0.2126 * r + 0.7152 * g + 0.0722 * b
  where r = f val.r
        g = f val.g
        b = f val.b

        f c | c <= 0.03928 = c / 12.92
            | otherwise    = ((c + 0.055) / 1.055) `pow` 2.4

        val = toRGBA' col

-- | The contrast ratio of two colors. A minimum contrast ratio of 4.5 is
-- | recommended to ensure that text is readable on a colored background. The
-- | contrast ratio is symmetric on both arguments:
-- | `contrast c1 c2 == contrast c2 c1`.
-- |
-- | See http://www.w3.org/TR/2008/REC-WCAG20-20081211/#contrast-ratiodef
contrast :: Color -> Color -> Number
contrast c1 c2 = if l1 > l2
                   then (l1 + o) / (l2 + o)
                   else (l2 + o) / (l1 + o)
  where l1 = luminance c1
        l2 = luminance c2
        o = 0.05

-- | Determine whether a color is perceived as a light color.
-- |
-- | ``` purs
-- | isLight c = brightness c > 0.5
-- | ```
isLight :: Color -> Boolean
isLight c = brightness c > 0.5

-- | Determine whether text of one color is readable on a background of a
-- | different color (see `contrast`). This function is symmetric in both
-- | arguments.
-- |
-- | ``` purs
-- | isReadable c1 c2 = contrast c1 c2 > 4.5
-- | ```
isReadable :: Color -> Color -> Boolean
isReadable c1 c2 = contrast c1 c2 > 4.5

-- | Return a readable foreground text color (either `black` or `white`) for a
-- | given background color.
textColor :: Color -> Color
textColor c | isLight c = black
            | otherwise = white

-- | Compute the perceived 'distance' between two colors according to the CIE76
-- | delta-E standard. A distance below ~2.3 is not noticable.
-- |
-- | See: https://en.wikipedia.org/wiki/Color_difference
distance :: Color -> Color -> Number
distance col1 col2 = sqrt (sq (c1.l - c2.l) + sq (c1.a - c2.a) + sq (c1.b - c2.b))
  where
    c1 = toLab col1
    c2 = toLab col2
    sq x = x `pow` 2.0
