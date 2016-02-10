module Test.Main where

import Prelude

import Data.Array ((..))
import Data.Foldable (sequence_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))

import Test.Unit (test, runTest)
import Test.Unit.Assert (assert, assertFalse, equal)

import Color
import Color.Blending
import Color.Gradient
import Color.Scheme.X11

main = runTest do
  test "Eq instance" do
    equal (hsl 120.0 0.3 0.5) (hsl 120.0 0.3 0.5)
    equal (rgba 1 2 3 0.3) (rgba 1 2 3 0.3)
    equal black (hsl 123.0 0.3 0.0)
    equal white (hsl 123.0 0.3 1.0)
    assertFalse "should not equal 1" $ hsl 120.0 0.3 0.5 == hsl 122.0 0.3 0.5
    assertFalse "should not equal 2" $ hsl 120.0 0.3 0.5 == hsl 120.0 0.32 0.5
    assertFalse "should not equal 3" $ rgba 1 2 3 0.4 == rgba 2 2 3 0.4
    assertFalse "should not equal 4" $ rgba 1 2 3 0.4 == rgba 1 1 3 0.4
    assertFalse "should not equal 5" $ rgba 1 2 3 0.4 == rgba 1 2 4 0.4
    assertFalse "should not equal 7" $ rgba 1 2 3 0.3 == rgba 1 2 3 0.30001

  test "rgba / rgb (RGB to HSL conversion)" do
    equal (hsl 0.0 0.0 1.0) (rgb' 1.0 1.0 1.0) -- white
    equal (hsl 0.0 0.0 0.5) (rgb' 0.5 0.5 0.5) -- gray
    equal (hsl 0.0 0.0 0.0) (rgb' 0.0 0.0 0.0) -- black
    equal (hsl 0.0 1.0 0.5) (rgb' 1.0 0.0 0.0) -- red
    equal (hsl 60.0 1.0 0.375) (rgb' 0.75 0.75 0.0) -- yellow-green
    equal (hsl 120.0 1.0 0.25) (rgb' 0.0 0.5 0.0) -- green
    equal (hsl 240.0 1.0 0.75) (rgb' 0.5 0.5 1.0) -- blue
    equal (hsl 49.5 0.893 0.497) (rgb' 0.941 0.785 0.053) -- yellow
    equal (hsl 162.4 0.779 0.447) (rgb' 0.099 0.795 0.591) -- cyan 2

  let roundtrip h s l = equal color color'
        where
          color = hsl h s l
          color' = case toRGBA color
            of { r, g, b, a } -> rgba r g b a

  test "toRGBA (HSL to RGB to HSL conversion)" do
    roundtrip 0.0 0.0 1.0
    roundtrip 0.0 0.0 0.5
    roundtrip 0.0 0.0 0.0
    roundtrip 0.0 1.0 0.5
    roundtrip 60.0 1.0 0.375
    roundtrip 120.0 1.0 0.25
    roundtrip 240.0 1.0 0.75
    roundtrip 49.5 0.893 0.497
    roundtrip 162.4 0.779 0.447
    sequence_ do
      degree <- 0 .. 360
      return $ roundtrip (toNumber degree) 0.5 0.8

  test "fromHexString" do
    equal (Just black) (fromHexString "#000")
    equal (Just black) (fromHexString "#000000")
    equal (Just white) (fromHexString "#fff")
    equal (Just white) (fromHexString "#fffFFF")
    equal (Just white) (fromHexString "#ffffff")
    equal (Just (rgb 87 166 206)) (fromHexString "#57A6CE")
    equal Nothing (fromHexString "000")
    equal Nothing (fromHexString "000000")
    equal Nothing (fromHexString "#0")
    equal Nothing (fromHexString "#00")
    equal Nothing (fromHexString "#0000")
    equal Nothing (fromHexString "#00000")
    equal Nothing (fromHexString "#0000000")

  test "toHexString" do
    let hexRoundtrip h s l = equal (Just $ hsl h s l) (fromHexString (toHexString (hsl h s l)))
    hexRoundtrip 0.0 0.0 1.0
    hexRoundtrip 0.0 0.0 0.5
    hexRoundtrip 0.0 0.0 0.0
    hexRoundtrip 0.0 1.0 0.5
    hexRoundtrip 60.0 1.0 0.375
    hexRoundtrip 120.0 1.0 0.25
    hexRoundtrip 240.0 1.0 0.75
    hexRoundtrip 49.5 0.893 0.497
    hexRoundtrip 162.4 0.779 0.447

  test "cssStringHSLA" do
    equal "hsla(120.1, 33.0%, 55.0%, 0.3)" (cssStringHSLA (hsla 120.1 0.33 0.55 0.3))
    equal "hsl(120.1, 33.2%, 54.9%)" (cssStringHSLA (hsla 120.1 0.332 0.549 1.0))

  test "complementary" do
    equal magenta (complementary lime)
    equal cyan (complementary red)
    equal yellow (complementary blue)

  test "lighten, darken" do
    equal white (lighten 1.0 black)
    equal black (darken 1.0 green)
    equal green (darken 0.0 green)

  test "saturate, desaturate" do
    equal (grayscale 0.5) (desaturate 1.0 red)
    equal (grayscale 0.5) (desaturate 1.0 magenta)

  test "brightness" do
    equal 1.0 (brightness white)
    equal 0.5 (brightness (grayscale 0.5))
    equal 0.0 (brightness black)

  -- Color.Blending

  test "blend" do
    let b = rgb 255 102 0
        f = rgb 51 51 51
    equal (blend Multiply b f) (rgb 51 20 0)
    equal (blend Screen b f) (rgb 255 133 51)
    equal (blend Overlay b f) (rgb 255 41 0)
    equal (blend Average b f) (rgb 153 77 26)

  -- Color.Gradient

  test "hslGradient" do
    equal [black, white] (hslGradient 0 black white)
    equal [black, grayscale 0.5, white] (hslGradient 3 black white)
    equal [white, grayscale 0.5, black] (hslGradient 3 white black)

    let c1 = hsl 40.0 0.3 0.6
        c2 = hsl 60.0 0.4 0.7
        c3 = hsl 80.0 0.5 0.8
    equal [c1, c2, c3] (hslGradient 3 c1 c3)
    equal [c3, c2, c1] (hslGradient 3 c3 c1)

    let c4 = hsl  20.0 0.3 0.6
        c5 = hsl   0.0 0.4 0.7
        c6 = hsl 340.0 0.5 0.8
    equal [c4, c5, c6] (hslGradient 3 c4 c6)
    equal [c6, c5, c4] (hslGradient 3 c6 c4)
