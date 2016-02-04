## Module Color

This module provides types and functions for dealing with colors, such as
the conversion between different color spaces and color manipulations.

#### `Color`

``` purescript
data Color
```

The representation of a color.

##### Instances
``` purescript
Show Color
Eq Color
```

#### `rgba`

``` purescript
rgba :: Int -> Int -> Int -> Number -> Color
```

Create a `Color` from integer RGB values between 0 and 255 and a floating
point alpha value between 0.0 and 1.0.

#### `rgb`

``` purescript
rgb :: Int -> Int -> Int -> Color
```

Create a `Color` from RGB values between 0 and 255.

#### `rgba'`

``` purescript
rgba' :: Number -> Number -> Number -> Number -> Color
```

Create a `Color` from RGB values between 0.0 and 1.0 and an alpha value
between 0.0 and 1.0.

#### `rgb'`

``` purescript
rgb' :: Number -> Number -> Number -> Color
```

Create a `Color` from RGB values between 0.0 and 1.0.

#### `hsla`

``` purescript
hsla :: Number -> Number -> Number -> Number -> Color
```

Create a `Color` from hue, saturation, lightness and alpha values.

#### `hsl`

``` purescript
hsl :: Number -> Number -> Number -> Color
```

Create a `Color` from hue, saturation and lightness values.

#### `black`

``` purescript
black :: Color
```

The color black.

#### `white`

``` purescript
white :: Color
```

The color white.

#### `grayscale`

``` purescript
grayscale :: Number -> Color
```

Create a gray tone from a lightness values (0.0 is black, 1.0 is white).

#### `toHSLA`

``` purescript
toHSLA :: Color -> { h :: Number, s :: Number, l :: Number, a :: Number }
```

Convert a `Color` to its hue, saturation, lightness and alpha values.

#### `toRGBA`

``` purescript
toRGBA :: Color -> { r :: Int, g :: Int, b :: Int, a :: Number }
```

Convert a `Color` to its red, green, blue and alpha values. The RGB values
are integers in the range from 0 to 255.

#### `toRGBA'`

``` purescript
toRGBA' :: Color -> { r :: Number, g :: Number, b :: Number, a :: Number }
```

Convert a `Color` to its red, green, blue and alpha values. All values
are numbers in the range from 0.0 to 1.0.

#### `cssStringHSLA`

``` purescript
cssStringHSLA :: Color -> String
```

The CSS representation of the color in the form `hsl(..)` or `hsla(...)`.

#### `complementary`

``` purescript
complementary :: Color -> Color
```

Get the complementary color (hue rotated by 180°).

#### `lighten`

``` purescript
lighten :: Number -> Color -> Color
```

Lighten a color by adding a certain amount (number between -1.0 and 1.0)
to the lightness channel. If the number is negative, the color is
darkened.

#### `darken`

``` purescript
darken :: Number -> Color -> Color
```

Darken a color by subtracting a certain amount (number between -1.0 and
1.0) from the lightness channel. If the number is negative, the color is
lightened.

#### `saturate`

``` purescript
saturate :: Number -> Color -> Color
```

Increase the saturation of a color by adding a certain amount (number
between -1.0 and 1.0) to the saturation channel. If the number is
negative, the color is desaturated.

#### `desaturate`

``` purescript
desaturate :: Number -> Color -> Color
```

Decrease the saturation of a color by subtracting a certain amount (number
between -1.0 and 1.0) from the saturation channel. If the number is
negative, the color is saturated.

#### `brightness`

``` purescript
brightness :: Color -> Number
```

The percieved brightness of the color (A number between 0.0 and 1.0).
See: https://www.w3.org/TR/AERT#color-contrast

#### `isLight`

``` purescript
isLight :: Color -> Boolean
```

Determine whether a color is perceived as a light color.


