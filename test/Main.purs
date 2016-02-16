module Test.Main where

import Prelude

import Data.Array ((..))
import Data.Foldable (sequence_)
import Data.List (List(..), (:))
import Data.Int (toNumber, round)
import Data.Maybe (Maybe(..))

import Test.Unit (test, runTest)
import Test.Unit.Assert (assertFalse, equal)

import Color
import Color.Blending
import Color.Scale
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

  test "rgb / rgba (RGB -> HSL conversion)" do
    equal (hsl 0.0 0.0 1.0) (rgb' 1.0 1.0 1.0) -- white
    equal (hsl 0.0 0.0 0.5) (rgb' 0.5 0.5 0.5) -- gray
    equal (hsl 0.0 0.0 0.0) (rgb' 0.0 0.0 0.0) -- black
    equal (hsl 0.0 1.0 0.5) (rgb' 1.0 0.0 0.0) -- red
    equal (hsl 60.0 1.0 0.375) (rgb' 0.75 0.75 0.0) -- yellow-green
    equal (hsl 120.0 1.0 0.25) (rgb' 0.0 0.5 0.0) -- green
    equal (hsl 240.0 1.0 0.75) (rgb' 0.5 0.5 1.0) -- blue
    equal (hsl 49.5 0.893 0.497) (rgb' 0.941 0.785 0.053) -- yellow
    equal (hsl 162.4 0.779 0.447) (rgb' 0.099 0.795 0.591) -- cyan 2

  test "fromHexString" do
    equal (Just black) (fromHexString "#000")
    equal (Just black) (fromHexString "#000000")
    equal (Just white) (fromHexString "#fff")
    equal (Just white) (fromHexString "#fffFFF")
    equal (Just white) (fromHexString "#ffffff")
    equal (Just red)   (fromHexString "#f00")
    equal (Just (rgb 87 166 206)) (fromHexString "#57A6CE")
    equal Nothing (fromHexString "000")
    equal Nothing (fromHexString "000000")
    equal Nothing (fromHexString "#0")
    equal Nothing (fromHexString "#00")
    equal Nothing (fromHexString "#0000")
    equal Nothing (fromHexString "#00000")
    equal Nothing (fromHexString "#0000000")

  test "fromInt" do
    equal black (fromInt 0)
    equal white (fromInt 0xffffff)
    equal black (fromInt (-1))
    equal white (fromInt (0xffffff + 1))
    equal (fromHexString "#123456") (Just (fromInt 0x123456))
    equal (fromHexString "#abcdef") (Just (fromInt 0xabcdef))

  let roundtrip h s l = equal color color'
        where
          color = hsl h s l
          color' = case toRGBA color
            of { r, g, b, a } -> rgba r g b a

  test "toRGBA (HSL -> RGB -> HSL conversion)" do
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

  test "toHexString (HSL -> Hex -> HSL conversion)" do
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

  test "cssStringRGB" do
    equal "rgba(42, 103, 255, 0.3)" (cssStringRGBA (rgba 42 103 255 0.3))

  test "graytone" do
    equal black (graytone 0.0)
    equal white (graytone 1.0)

  test "rotateHue" do
    equal black (rotateHue 123.0 black)

  test "complementary" do
    equal magenta (complementary lime)
    equal cyan (complementary red)
    equal yellow (complementary blue)

  test "lighten, darken" do
    equal white (lighten 1.0 black)
    equal black (darken 1.0 green)
    equal green (darken 0.0 green)

  test "saturate, desaturate" do
    equal (graytone 0.5) (desaturate 1.0 red)
    equal (graytone 0.5) (desaturate 1.0 magenta)

  test "mix" do
    equal (fromInt 0x800080) (mix RGB red blue 0.5)
    equal (fromInt 0xff00ff) (mix HSL red blue 0.5)

  test "brightness" do
    equal 1.0 (brightness white)
    equal 0.5 (brightness (graytone 0.5))
    equal 0.0 (brightness black)

  test "luminance" do
    equal 1.0 (luminance white)
    equal 808 (round (1000.0 * luminance aquamarine))
    equal 347 (round (1000.0 * luminance hotpink))
    equal  66 (round (1000.0 * luminance darkslateblue))
    equal 0.0 (luminance black)

  test "contrast" do
    equal 21.0 (contrast black white)
    equal 1.0 (contrast white white)
    equal 1721 (round (1000.0 * contrast pink hotpink))
    equal 6124 (round (1000.0 * contrast pink purple))

  test "textColor" do
    equal black (textColor $ graytone 0.6)
    equal white (textColor $ graytone 0.4)

  -- Color.Blending

  test "blend" do
    let b = rgb 255 102 0
        f = rgb 51 51 51
    equal (rgb 51 20 0) (blend Multiply b f)
    equal (rgb 255 133 51) (blend Screen b f)
    equal (rgb 255 41 0) (blend Overlay b f)

  -- Color.Scale

  let scale = colorScale HSL red (colorStop blue 0.3 : Nil) yellow

  test "colorScale, sample" do
    equal red    (sample scale (-10.0))
    equal red    (sample scale 0.0)
    equal red    (sample scale 0.0001)
    equal blue   (sample scale 0.2999)
    equal blue   (sample scale 0.3)
    equal blue   (sample scale 0.3001)
    equal yellow (sample scale 0.9999)
    equal yellow (sample scale 1.0)
    equal yellow (sample scale 10.0)

    equal (mix HSL red blue 0.5)    (sample scale 0.15)
    equal (mix HSL blue yellow 0.5) (sample scale 0.65)

  test "colors" do
    equal (Nil)                (colors scale 0)
    equal (red : Nil)          (colors scale 1)
    equal (red : yellow : Nil) (colors scale 2)
    equal (black : graytone 0.25 : graytone 0.5 : graytone 0.75 : white : Nil)
          (colors grayscale 5)

  test "grayscale" do
    equal black (sample grayscale 0.0)
    equal white (sample grayscale 1.0)

  test "Misc" do
    equal "#36e985" (seagreen # lighten 0.2 # saturate 0.3 # toHexString)
    equal 0.5 (orangered # toHSLA # _.l)
    equal 69 (orangered # toRGBA # _.g)
    equal "#33cc00" (toHexString $ rgb' 0.2 0.8 0.0)
