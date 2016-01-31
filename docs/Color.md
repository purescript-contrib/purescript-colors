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

Create a `Color` from hue, saturation, lightness and alpha values.

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

Convert a `Color` to its red, green, blue and alpha values.

#### `cssStringHSLA`

``` purescript
cssStringHSLA :: Color -> String
```


