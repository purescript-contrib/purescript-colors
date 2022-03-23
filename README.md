# Colors

[![CI](https://github.com/purescript-contrib/purescript-colors/workflows/CI/badge.svg?branch=main)](https://github.com/purescript-contrib/purescript-colors/actions?query=workflow%3ACI+branch%3Amain)
[![Release](https://img.shields.io/github/release/purescript-contrib/purescript-colors.svg)](https://github.com/purescript-contrib/purescript-colors/releases)
[![Pursuit](https://pursuit.purescript.org/packages/purescript-colors/badge)](https://pursuit.purescript.org/packages/purescript-colors)

A package for dealing with colors in PureScript.

## Installation

Install `colors` with [Spago](https://github.com/purescript/spago):

```sh
spago install colors
```

## Quick start

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

## Documentation

Otherwise, `colors` documentation is stored in a few places:

1. Module documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-colors).
1. Written documentation is kept in the [docs directory](./docs).
1. Usage examples can be found in [the test suite](./test).

If you get stuck, there are several ways to get help:

- [Open an issue](https://github.com/purescript-contrib/purescript-colors/issues) if you have encountered a bug or problem.
- Ask general questions on the [PureScript Discourse](https://discourse.purescript.org) forum or the [PureScript Discord](https://discord.com/invite/sMqwYUbvz6) chat.

## Contributing

You can contribute to `colors` in several ways:

1. If you encounter a problem or have a question, please [open an issue](https://github.com/purescript-contrib/purescript-colors/issues). We'll do our best to work with you to resolve or answer it.

2. If you would like to contribute code, tests, or documentation, please [read the contributor guide](./CONTRIBUTING.md). It's a short, helpful introduction to contributing to this library, including development instructions.

3. If you have written a library, tutorial, guide, or other resource based on this package, please share it on the [PureScript Discourse](https://discourse.purescript.org)! Writing libraries and learning resources are a great way to help this library succeed.

## Credit

Based on initial work by paf31 and inspired by (the PureScript version of) [Elm.Color](https://github.com/brainrape/purescript-elm-color), [TinyColor](https://github.com/bgrins/TinyColor) and [Chroma.js](https://github.com/gka/chroma.js).
