module Test.Main where

import Prelude

import Data.Array ((..))
import Data.Foldable (sequence_)
import Data.Int (toNumber)

import Test.Unit (test, runTest)
import Test.Unit.Assert (assert, assertFalse, equal)

import Color
import Color.Blending

main = runTest do
  test "Eq instance" do
    equal (hsl 120.0 0.3 0.5) (hsl 120.0 0.3 0.5)
    equal (rgba 1 2 3 0.3) (rgba 1 2 3 0.3)
    equal (rgba 1 2 3 0.3) (rgba 1 2 3 0.30001)
    assertFalse "should not equal 1" $ hsl 120.0 0.3 0.5 == hsl 122.0 0.3 0.5
    assertFalse "should not equal 2" $ hsl 120.0 0.3 0.5 == hsl 120.0 0.32 0.5
    assertFalse "should not equal 3" $ rgba 1 2 3 0.4 == rgba 2 2 3 0.4
    assertFalse "should not equal 4" $ rgba 1 2 3 0.4 == rgba 1 1 3 0.4
    assertFalse "should not equal 5" $ rgba 1 2 3 0.4 == rgba 1 2 4 0.4
    assertFalse "should not equal 6" $ rgba 1 2 3 0.4 == rgba 1 2 3 0.45

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

  test "cssStringHSLA" do
    equal "hsla(120, 33%, 55%, 0.3)" (cssStringHSLA (hsla 120.1 0.33 0.55 0.3))
    equal "hsl(120, 33%, 55%)" (cssStringHSLA (hsla 120.1 0.332 0.549 1.0))

  test "blend" do
    let b = rgb 255 102 0
        f = rgb 51 51 51
    equal (blend Multiply b f) (rgb 51 20 0)
    equal (blend Screen b f) (rgb 255 133 51)
    equal (blend Overlay b f) (rgb 255 41 0)
    equal (blend Average b f) (rgb 153 77 26)
