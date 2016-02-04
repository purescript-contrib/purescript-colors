module Test.Interactive where

import Prelude

import Text.Smolder.Markup as H
import Text.Smolder.Markup ((!), text)
import Text.Smolder.HTML as H
import Text.Smolder.HTML.Attributes as HA
import Text.Smolder.Renderer.String as H

import Test.FlareDoc

import Color
import Color.Scheme.MaterialDesign as MD
import Color.Scheme.X11

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
    doc "black" (TColor black)
    doc "white" (TColor white)
    doc "grayscale" (\(Number1 s) -> TColor (grayscale s))
    doc "complementary" (\(TColor c) -> TColor (complementary c))
    doc "lighten"    (\(Number1 a) (TColor c) -> TColor (lighten a c))
    doc "darken"     (\(Number1 a) (TColor c) -> TColor (darken a c))
    doc "saturate"   (\(Number1 a) (TColor c) -> TColor (saturate a c))
    doc "desaturate" (\(Number1 a) (TColor c) -> TColor (desaturate a c))

    let docmd :: forall t. Interactive t => String -> t -> _
        docmd = flareDoc' "doc-scheme-md" dict "Color.Scheme.MaterialDesign"

    docmd "red" (TColor MD.red)
    docmd "pink" (TColor MD.pink)
    docmd "purple" (TColor MD.purple)
    docmd "deepPurple" (TColor MD.deepPurple)
    docmd "indigo" (TColor MD.indigo)
    docmd "blue" (TColor MD.blue)
    docmd "lightBlue" (TColor MD.lightBlue)
    docmd "cyan" (TColor MD.cyan)
    docmd "teal" (TColor MD.teal)
    docmd "green" (TColor MD.green)
    docmd "lightGreen" (TColor MD.lightGreen)
    docmd "lime" (TColor MD.lime)
    docmd "yellow" (TColor MD.yellow)
    docmd "amber" (TColor MD.amber)
    docmd "orange" (TColor MD.orange)
    docmd "deepOrange" (TColor MD.deepOrange)
    docmd "brown" (TColor MD.brown)
    docmd "grey" (TColor MD.grey)
    docmd "blueGrey" (TColor MD.blueGrey)

    let docx11 :: forall t. Interactive t => String -> t -> _
        docx11 = flareDoc' "doc-scheme-x11" dict "Color.Scheme.X11"

    docx11 "aliceblue" (TColor aliceblue)
    docx11 "antiquewhite" (TColor antiquewhite)
    docx11 "aqua" (TColor aqua)
    docx11 "aquamarine" (TColor aquamarine)
    docx11 "azure" (TColor azure)
    docx11 "beige" (TColor beige)
    docx11 "bisque" (TColor bisque)
    docx11 "blanchedalmond" (TColor blanchedalmond)
    docx11 "blue" (TColor blue)
    docx11 "blueviolet" (TColor blueviolet)
    docx11 "brown" (TColor brown)
    docx11 "burlywood" (TColor burlywood)
    docx11 "cadetblue" (TColor cadetblue)
    docx11 "chartreuse" (TColor chartreuse)
    docx11 "chocolate" (TColor chocolate)
    docx11 "coral" (TColor coral)
    docx11 "cornflowerblue" (TColor cornflowerblue)
    docx11 "cornsilk" (TColor cornsilk)
    docx11 "crimson" (TColor crimson)
    docx11 "cyan" (TColor cyan)
    docx11 "darkblue" (TColor darkblue)
    docx11 "darkcyan" (TColor darkcyan)
    docx11 "darkgoldenrod" (TColor darkgoldenrod)
    docx11 "darkgray" (TColor darkgray)
    docx11 "darkgreen" (TColor darkgreen)
    docx11 "darkgrey" (TColor darkgrey)
    docx11 "darkkhaki" (TColor darkkhaki)
    docx11 "darkmagenta" (TColor darkmagenta)
    docx11 "darkolivegreen" (TColor darkolivegreen)
    docx11 "darkorange" (TColor darkorange)
    docx11 "darkorchid" (TColor darkorchid)
    docx11 "darkred" (TColor darkred)
    docx11 "darksalmon" (TColor darksalmon)
    docx11 "darkseagreen" (TColor darkseagreen)
    docx11 "darkslateblue" (TColor darkslateblue)
    docx11 "darkslategray" (TColor darkslategray)
    docx11 "darkslategrey" (TColor darkslategrey)
    docx11 "darkturquoise" (TColor darkturquoise)
    docx11 "darkviolet" (TColor darkviolet)
    docx11 "deeppink" (TColor deeppink)
    docx11 "deepskyblue" (TColor deepskyblue)
    docx11 "dimgray" (TColor dimgray)
    docx11 "dimgrey" (TColor dimgrey)
    docx11 "dodgerblue" (TColor dodgerblue)
    docx11 "firebrick" (TColor firebrick)
    docx11 "floralwhite" (TColor floralwhite)
    docx11 "forestgreen" (TColor forestgreen)
    docx11 "fuchsia" (TColor fuchsia)
    docx11 "gainsboro" (TColor gainsboro)
    docx11 "ghostwhite" (TColor ghostwhite)
    docx11 "gold" (TColor gold)
    docx11 "goldenrod" (TColor goldenrod)
    docx11 "gray" (TColor gray)
    docx11 "green" (TColor green)
    docx11 "greenyellow" (TColor greenyellow)
    docx11 "grey" (TColor grey)
    docx11 "honeydew" (TColor honeydew)
    docx11 "hotpink" (TColor hotpink)
    docx11 "indianred" (TColor indianred)
    docx11 "indigo" (TColor indigo)
    docx11 "ivory" (TColor ivory)
    docx11 "khaki" (TColor khaki)
    docx11 "lavender" (TColor lavender)
    docx11 "lavenderblush" (TColor lavenderblush)
    docx11 "lawngreen" (TColor lawngreen)
    docx11 "lemonchiffon" (TColor lemonchiffon)
    docx11 "lightblue" (TColor lightblue)
    docx11 "lightcoral" (TColor lightcoral)
    docx11 "lightcyan" (TColor lightcyan)
    docx11 "lightgoldenrodyellow" (TColor lightgoldenrodyellow)
    docx11 "lightgray" (TColor lightgray)
    docx11 "lightgreen" (TColor lightgreen)
    docx11 "lightgrey" (TColor lightgrey)
    docx11 "lightpink" (TColor lightpink)
    docx11 "lightsalmon" (TColor lightsalmon)
    docx11 "lightseagreen" (TColor lightseagreen)
    docx11 "lightskyblue" (TColor lightskyblue)
    docx11 "lightslategray" (TColor lightslategray)
    docx11 "lightslategrey" (TColor lightslategrey)
    docx11 "lightsteelblue" (TColor lightsteelblue)
    docx11 "lightyellow" (TColor lightyellow)
    docx11 "lime" (TColor lime)
    docx11 "limegreen" (TColor limegreen)
    docx11 "linen" (TColor linen)
    docx11 "magenta" (TColor magenta)
    docx11 "maroon" (TColor maroon)
    docx11 "mediumaquamarine" (TColor mediumaquamarine)
    docx11 "mediumblue" (TColor mediumblue)
    docx11 "mediumorchid" (TColor mediumorchid)
    docx11 "mediumpurple" (TColor mediumpurple)
    docx11 "mediumseagreen" (TColor mediumseagreen)
    docx11 "mediumslateblue" (TColor mediumslateblue)
    docx11 "mediumspringgreen" (TColor mediumspringgreen)
    docx11 "mediumturquoise" (TColor mediumturquoise)
    docx11 "mediumvioletred" (TColor mediumvioletred)
    docx11 "midnightblue" (TColor midnightblue)
    docx11 "mintcream" (TColor mintcream)
    docx11 "mistyrose" (TColor mistyrose)
    docx11 "moccasin" (TColor moccasin)
    docx11 "navajowhite" (TColor navajowhite)
    docx11 "navy" (TColor navy)
    docx11 "oldlace" (TColor oldlace)
    docx11 "olive" (TColor olive)
    docx11 "olivedrab" (TColor olivedrab)
    docx11 "orange" (TColor orange)
    docx11 "orangered" (TColor orangered)
    docx11 "orchid" (TColor orchid)
    docx11 "palegoldenrod" (TColor palegoldenrod)
    docx11 "palegreen" (TColor palegreen)
    docx11 "paleturquoise" (TColor paleturquoise)
    docx11 "palevioletred" (TColor palevioletred)
    docx11 "papayawhip" (TColor papayawhip)
    docx11 "peachpuff" (TColor peachpuff)
    docx11 "peru" (TColor peru)
    docx11 "pink" (TColor pink)
    docx11 "plum" (TColor plum)
    docx11 "powderblue" (TColor powderblue)
    docx11 "purple" (TColor purple)
    docx11 "red" (TColor red)
    docx11 "rosybrown" (TColor rosybrown)
    docx11 "royalblue" (TColor royalblue)
    docx11 "saddlebrown" (TColor saddlebrown)
    docx11 "salmon" (TColor salmon)
    docx11 "sandybrown" (TColor sandybrown)
    docx11 "seagreen" (TColor seagreen)
    docx11 "seashell" (TColor seashell)
    docx11 "sienna" (TColor sienna)
    docx11 "silver" (TColor silver)
    docx11 "skyblue" (TColor skyblue)
    docx11 "slateblue" (TColor slateblue)
    docx11 "slategray" (TColor slategray)
    docx11 "slategrey" (TColor slategrey)
    docx11 "snow" (TColor snow)
    docx11 "springgreen" (TColor springgreen)
    docx11 "steelblue" (TColor steelblue)
    docx11 "tan" (TColor tan)
    docx11 "teal" (TColor teal)
    docx11 "thistle" (TColor thistle)
    docx11 "tomato" (TColor tomato)
    docx11 "turquoise" (TColor turquoise)
    docx11 "violet" (TColor violet)
    docx11 "wheat" (TColor wheat)
    docx11 "whitesmoke" (TColor whitesmoke)
    docx11 "yellow" (TColor yellow)
    docx11 "yellowgreen" (TColor yellowgreen)
