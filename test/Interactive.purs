module Test.Interactive where

import Prelude

import Text.Smolder.Markup as H
import Text.Smolder.Markup ((!), text)
import Text.Smolder.HTML as H
import Text.Smolder.HTML.Attributes as HA
import Text.Smolder.Renderer.String as H

import Test.FlareDoc

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
  withPackage "purescript-colors.json" $ \dict -> do
    let doc :: forall t. Interactive t => String -> t -> _
        doc = flareDoc' "doc-color" dict "Color"

    doc "hsla" (id :: TColor -> _)
    doc "rgba" $
      (\(Int255 r) (Int255 g) (Int255 b) (Number1 a) -> TColor (rgba r g b a))
    doc "cssStringHSLA" (\(TColor c) -> cssStringHSLA c)
    doc "grayscale" (\(Number1 s) -> TColor (grayscale s))
    doc "complementary" (\(TColor c) -> TColor (complementary c))
    doc "lighten" (\(Number1 a) (TColor c) -> TColor (lighten a c))
    doc "darken" (\(Number1 a) (TColor c) -> TColor (darken a c))

    let docmd :: forall t. Interactive t => String -> t -> _
        docmd = flareDoc' "doc-scheme-md" dict "Color.Scheme.MaterialDesign"

    docmd "red" (TColor red)
    docmd "pink" (TColor pink)
    docmd "purple" (TColor purple)
    docmd "deepPurple" (TColor deepPurple)
    docmd "indigo" (TColor indigo)
    docmd "blue" (TColor blue)
    docmd "lightBlue" (TColor lightBlue)
    docmd "cyan" (TColor cyan)
    docmd "teal" (TColor teal)
    docmd "green" (TColor green)
    docmd "lightGreen" (TColor lightGreen)
    docmd "lime" (TColor lime)
    docmd "yellow" (TColor yellow)
    docmd "amber" (TColor amber)
    docmd "orange" (TColor orange)
    docmd "deepOrange" (TColor deepOrange)
    docmd "brown" (TColor brown)
    docmd "grey" (TColor grey)
    docmd "blueGrey" (TColor blueGrey)
