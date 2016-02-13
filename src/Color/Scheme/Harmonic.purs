module Color.Scheme.Harmonic
  ( shades
  , analogous
  , triad
  , splitComplementary
  , tetrad
  ) where

import Prelude

import Color (Color, rotateHue, darken, lighten)

-- | The given color plus a lighter and darker variant.
shades :: Color -> Array Color
shades c =
  [ lighten 0.2 c
  , c
  , darken 0.2 c
  ]

-- | Three colors, separated by 30° in hue and centered around the given
-- | color.
analogous :: Color -> Array Color
analogous c =
  [ rotateHue (-30.0) c
  , c
  , rotateHue 30.0 c
  ]

-- | Three colors that are evenly spaced along the HSL color wheel.
triad :: Color -> Array Color
triad c =
  [ c
  , rotateHue (-120.0) c
  , rotateHue 120.0 c
  ]

-- | The given color and two complementary colors, split by 30°.
splitComplementary :: Color -> Array Color
splitComplementary c =
  [ c
  , rotateHue (-165.0) c
  , rotateHue 165.0 c
  ]

-- | Four colors that come in groups of two pairs which are on opposite sides
-- | of the color wheel.
tetrad :: Color -> Array Color
tetrad c =
  [ c
  , rotateHue 30.0 c
  , rotateHue 180.0 c
  , rotateHue 210.0 c
  ]
