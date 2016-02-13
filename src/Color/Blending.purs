module Color.Blending
  ( BlendMode(..)
  , blend
  ) where

import Prelude

import Color (Color, rgba', toRGBA')

data BlendMode = Multiply | Screen | Overlay

-- | Blend two RGB channel values (numbers between 0.0 and 1.0).
blendChannel :: BlendMode -> Number -> Number -> Number
blendChannel Multiply b f = b * f
blendChannel Screen   b f = 1.0 - (1.0 - b) * (1.0 - f)
blendChannel Overlay  b f | b < 0.5   = 2.0 * b * f
                          | otherwise = 1.0 - 2.0 * (1.0 - b) * (1.0 - f)

-- | Blend two colors with a specified blend mode. The first color is the
-- | background color, the second one is the foreground color. The resulting
-- | alpha value is calculated as arithmetic mean.
blend :: BlendMode -> Color -> Color -> Color
blend mode c1 c2 = rgba' r g b a
  where
    v1 = toRGBA' c1
    v2 = toRGBA' c2
    r = blendChannel mode v1.r v2.r
    g = blendChannel mode v1.g v2.g
    b = blendChannel mode v1.b v2.b
    a = (v1.a + v2.a) / 2.0
