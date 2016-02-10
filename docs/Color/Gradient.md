## Module Color.Gradient

#### `hslGradient`

``` purescript
hslGradient :: Int -> Color -> Color -> Array Color
```

A range of colors that linearly interpolates between the two boundaries.
The interpolation uses the HSL colorspace and always takes the shortest
path along the circle of hue values.


