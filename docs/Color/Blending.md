## Module Color.Blending

#### `BlendMode`

``` purescript
data BlendMode
  = Multiply
  | Screen
  | Overlay
  | Average
```

#### `blend`

``` purescript
blend :: BlendMode -> Color -> Color -> Color
```

Blend two colors with a specified blend mode. The first color is the
background color, the second one is the foreground color. The resulting
alpha value is calculated as arithmetic mean.


