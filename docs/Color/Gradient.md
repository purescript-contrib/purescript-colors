## Module Color.Gradient

#### `InterpolationMode`

``` purescript
data InterpolationMode
  = HSL
  | RGB
```

Defines the colorspace in which an interpolation takes place.

#### `linearGradient`

``` purescript
linearGradient :: InterpolationMode -> Int -> Color -> Color -> Array Color
```

A range of colors that linearly interpolates between the two boundaries.
The interpolation uses the defined colorspace. For the HSL colorspace, the
shortest path is chosen along the circle of hue values. The total number
of colors can be specified.

#### `interpolate`

``` purescript
interpolate :: InterpolationMode -> Color -> Color -> Number -> Color
```

Linearly interpolate between two colors, using the specified color space.


