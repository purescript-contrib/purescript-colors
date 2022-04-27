# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:

New features:

Bugfixes:

Other improvements:

## [v7.0.1](https://github.com/purescript-contrib/purescript-colors/releases/tag/v7.0.1) - 2022-04-27

Fix registry URL, so we can publish package (#61 by @JordanMartinez)
## [v7.0.0](https://github.com/purescript-contrib/purescript-colors/releases/tag/v7.0.0) - 2022-04-27

Breaking changes:
- Update project and deps to PureScript v0.15.0 (#51 by @JordanMartinez)

New features:
- Support for alpha channel added to `toHexString` and `fromHexString` (#56 by @nsaunders)

Bugfixes:

Other improvements:
- Drop dependency on `math` (#54 by @JordanMartinez)

## [v5.0.0](https://github.com/purescript-contrib/purescript-colors/releases/tag/v5.0.0) - 2018-05-27

Updates for PS 0.12 (Thanks to @justinwoo!)

## [v4.3.0](https://github.com/purescript-contrib/purescript-colors/releases/tag/v4.3.0) - 2017-10-04

* Added `combineStops` and `reverseStops` (@safareli)

## [v4.0.0](https://github.com/purescript-contrib/purescript-colors/releases/tag/v4.0.0) - 2017-08-31

- Added cubehelix support (sampling functions, color scale) (@safareli)
- Do not clip hue angles in the internal `Color` representation (@safareli)

## [v3.1.0](https://github.com/purescript-contrib/purescript-colors/releases/tag/v3.1.0) - 2017-08-02

- Added support for HSV: `hsva`, `hsv`, `toHSV` (@safareli)
- Moved interactive tests to sub-folder, making it easier to run the unit tests (@safareli)

## [v3.0.0](https://github.com/purescript-contrib/purescript-colors/releases/tag/v3.0.0) - 2017-04-03

- Updates for PureScript 0.11.x (@paf31)

## [v2.2.0](https://github.com/purescript-contrib/purescript-colors/releases/tag/v2.2.0) - 2017-03-06

- Added [Clrs color scheme](http://clrs.cc/)

## [v2.1.0](https://github.com/purescript-contrib/purescript-colors/releases/tag/v2.1.0) - 2016-11-27

- Fix shadowed name warnings (@garyb)

## [v2.0.0](https://github.com/purescript-contrib/purescript-colors/releases/tag/v2.0.0) - 2016-10-25

- Updates for PureScript 0.10.1 (@paf31)

## [v1.0.2](https://github.com/purescript-contrib/purescript-colors/releases/tag/v1.0.2) - 2016-07-28

- Update interactive documentation to PureScript 0.9

## [v1.0.1](https://github.com/purescript-contrib/purescript-colors/releases/tag/v1.0.1) - 2016-07-28

- Fixed bug in hexadecimal conversion

## [v1.0.0](https://github.com/purescript-contrib/purescript-colors/releases/tag/v1.0.0) - 2016-06-12

- Updates to PureScript 0.9 (@paf31)
- purescript-colors is now JavaScript-free.. at least superficially ;-)

## [v0.4.4](https://github.com/purescript-contrib/purescript-colors/releases/tag/v0.4.4) - 2016-05-04

- Add MIT license for Pursuit

## [v0.4.3](https://github.com/purescript-contrib/purescript-colors/releases/tag/v0.4.3) - 2016-02-28

- Add `distance`: compute CIE76 color differences
- New color distance interactive demo
- Fix a bug in `uniformScale`
- Two new color scales: `blueToRed`, `yellowToRed`

## [v0.4.2](https://github.com/purescript-contrib/purescript-colors/releases/tag/v0.4.2) - 2016-02-28

- Add `toGray`
- `ColorScale`: Add `modify`, add `spectrumLCh`
- Updated documentation

## [v0.4.1](https://github.com/purescript-contrib/purescript-colors/releases/tag/v0.4.1) - 2016-02-21

- New color spaces: Lab, LCh, XYZ
- New palette-creator example

## [v0.4.0](https://github.com/purescript-contrib/purescript-colors/releases/tag/v0.4.0) - 2016-02-16

- Color scales!
  - CSS gradients (with HSL support!)
  - Sampling
  - Predefined scales (grayscale, spectrum, matplotlib scales)
- Improved documentation
- Added "playground"
- Removed `Gradient` module (replaced by the color scales)
- Renamed grayscale to graytone (grayscale is now a color scale)

## [v0.3.0](https://github.com/purescript-contrib/purescript-colors/releases/tag/v0.3.0) - 2016-02-14

- Add `textColor`
- Add `luminance`
- Add `cssStringRGBA`
- Add `fromInt`
- Add `mix`
- Add `contrast`

## [v0.2.0](https://github.com/purescript-contrib/purescript-colors/releases/tag/v0.2.0) - 2016-02-10

- Add `linearGradient` with `HSL` and `RGB` mode
- Add `Color.Scheme.Harmonic`, see #5 
- Add `rotateHue`
- Fix `Eq` instances, see #10 
- HSL gradients: take shortest path along color wheel
- Update tests

## [v0.1.5](https://github.com/purescript-contrib/purescript-colors/releases/tag/v0.1.5) - 2016-02-08

- Do not round HSL channels to integer values in `cssStringHSLA`. This caused "jumps" in the RGB values (bigger than 1).

## [v0.1.4](https://github.com/purescript-contrib/purescript-colors/releases/tag/v0.1.4) - 2016-02-07

- Add toHexString, fromHexString

## [v0.1.3](https://github.com/purescript-contrib/purescript-colors/releases/tag/v0.1.3) - 2016-02-04

- Color gradients
- Color blending
- Added `brightness` and `isLight`
- Added `toRGBA'`

## [v0.1.2](https://github.com/purescript-contrib/purescript-colors/releases/tag/v0.1.2) - 2016-02-04

- Add black, white
- Add Color.Scheme.X11 and Color.Scheme.HTML

## [v0.1.1](https://github.com/purescript-contrib/purescript-colors/releases/tag/v0.1.1) - 2016-02-04

- Add saturate, desaturate
- Interactive documentation

## [v0.1.0](https://github.com/purescript-contrib/purescript-colors/releases/tag/v0.1.0) - 2016-02-02

Initial release

