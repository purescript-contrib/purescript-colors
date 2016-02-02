module Test.Interactive where

import Prelude
import Test.FlareCheck

import Text.Smolder.Markup as H
import Text.Smolder.Markup ((!), text)
import Text.Smolder.HTML as H
import Text.Smolder.HTML.Attributes as HA
import Text.Smolder.Renderer.String as H

import Color
import Color.Scheme.MaterialDesign

newtype TColor = TColor Color

instance flammableTColor :: Flammable TColor where
  spark = TColor <$> fieldset "Color"
                     (hsla <$> numberSlider "Hue" 0.0 360.0 1.0 0.0
                      <*> numberSlider "Saturation" 0.0 1.0 0.01 0.8
                      <*> numberSlider "Lightness" 0.0 1.0 0.01 0.4
                      <*> numberSlider "Alpha" 0.0 1.0 0.01 1.0)

instance interactiveTColor :: Interactive TColor where
  interactive ui = (SetHTML <<< pretty) <$> ui
    where
      pretty (TColor c) = do
        H.div ! HA.style css $ H.text ""
        H.pre $ text $ repr

        where css = "background-color: " <> repr <> ";" <>
                    "width: 100%; height: 30px"
              repr = cssStringHSLA c

newtype Number1 = Number1 Number
newtype Int255 = Int255 Int

instance flammableNumber1 :: Flammable Number1 where
  spark = Number1 <$> numberSlider "Number" 0.0 1.0 0.01 0.3

instance flammableInt255 :: Flammable Int255 where
  spark = Int255 <$> intSlider "Int" 0 255 100

main = do
  flareCheck "hsla" (id :: TColor -> _)
  flareCheck "rgba :: Int -> Int -> Int -> Number -> Color" $
    (\(Int255 r) (Int255 g) (Int255 b) (Number1 a) -> TColor (rgba r g b a))
  flareCheck "cssStringHSLA" (\(TColor c) -> cssStringHSLA c)
  flareCheck "greyscale" (\(Number1 s) -> TColor (grayscale s))
  flareCheck "complementary" (\(TColor c) -> TColor (complementary c))
  flareCheck "lighten" (\(Number1 a) (TColor c) -> TColor (lighten a c))
  flareCheck "darken" (\(Number1 a) (TColor c) -> TColor (darken a c))

  flareCheck' "tests-md" "red" (TColor red)
  flareCheck' "tests-md" "pink" (TColor pink)
  flareCheck' "tests-md" "purple" (TColor purple)
  flareCheck' "tests-md" "deepPurple" (TColor deepPurple)
  flareCheck' "tests-md" "indigo" (TColor indigo)
  flareCheck' "tests-md" "blue" (TColor blue)
  flareCheck' "tests-md" "lightBlue" (TColor lightBlue)
  flareCheck' "tests-md" "cyan" (TColor cyan)
  flareCheck' "tests-md" "teal" (TColor teal)
  flareCheck' "tests-md" "green" (TColor green)
  flareCheck' "tests-md" "lightGreen" (TColor lightGreen)
  flareCheck' "tests-md" "lime" (TColor lime)
  flareCheck' "tests-md" "yellow" (TColor yellow)
  flareCheck' "tests-md" "amber" (TColor amber)
  flareCheck' "tests-md" "orange" (TColor orange)
  flareCheck' "tests-md" "deepOrange" (TColor deepOrange)
  flareCheck' "tests-md" "brown" (TColor brown)
  flareCheck' "tests-md" "grey" (TColor grey)
  flareCheck' "tests-md" "blueGrey" (TColor blueGrey)
