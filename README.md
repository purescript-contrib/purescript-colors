# purescript-colors

A package for dealing with colors in PureScript.

- [Interactive documentation](http://sharkdp.github.io/purescript-colors/)
- [Module documentation](https://pursuit.purescript.org/packages/purescript-colors)

## Examples

```purs
> toHexString (hsl 200.0 0.4 0.5)
"#4d91b3"

> cssStringHSLA (seagreen # lighten 0.2 # saturate 0.3)
"hsl(146.45, 80.27%, 56.27%)"

> toHexString <$> colors (colorScale HSL hotpink Nil darksalmon) 5
"#ff69b4" : "#fa6d99" : "#f47182" : "#ef7d76" : "#e9967a" : Nil

> toHexString <$> (sortBy (comparing luminance) [black, white, blue, lightgreen])
["#000000","#0000ff","#90ee90","#ffffff"]


```

## Credit

Based on initial work by paf31 and inspired by (the PureScript version of) [Elm.Color](https://github.com/brainrape/purescript-elm-color), [TinyColor](https://github.com/bgrins/TinyColor) and [Chroma.js](https://github.com/gka/chroma.js).
