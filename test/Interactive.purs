module Test.Interactive (main) where

import Prelude

import Data.Foldable (foldMap)
import Data.List (List(..), fromList)

import Text.Smolder.Markup as H
import Text.Smolder.Markup ((!))
import Text.Smolder.HTML as H
import Text.Smolder.HTML.Attributes as HA

import Flare
import Flare.Smolder
import Test.FlareDoc

import Color
import Color.Blending
import Color.Scale
import Color.Scale.Perceptual
import Color.Scheme.Harmonic
import Color.Scheme.MaterialDesign as MD
import Color.Scheme.X11

newtype TColor = TColor Color

runTColor :: TColor -> Color
runTColor (TColor c) = c

instance flammableTColor :: Flammable TColor where
  spark = TColor <$> fieldset "Color"
                     (hsl <$> numberSlider "Hue" 0.0 360.0 0.1 231.0
                          <*> numberSlider "Saturation" 0.0 1.0 0.001 0.48
                          <*> numberSlider "Lightness" 0.0 1.0 0.001 0.48)

colorBox :: Color -> H.Markup
colorBox c = H.div ! HA.style css $ H.code (H.text repr)
  where
    repr = cssStringHSLA c
    css = "background-color: " <> repr <> ";" <>
          "width: 260px; height: 50px; display: inline-block;" <>
          "margin-top: 10px; margin-right: 10px; border: 1px solid black;" <>
          "padding: 5px; color: " <> cssStringHSLA (textColor c)

instance interactiveTColor :: Interactive TColor where
  interactive ui = (SetHTML <<< colorBox <<< runTColor) <$> ui

data ColorList = ColorList (Array Color)

instance interactiveColorList :: Interactive ColorList where
  interactive ui = (SetHTML <<< pretty) <$> ui
    where
      pretty (ColorList cs) = foldMap colorBox cs

newtype TBlendMode = TBlendMode BlendMode

modeToString Multiply = "Multiply"
modeToString Screen = "Screen"
modeToString Overlay = "Overlay"

instance flammableTBlendMode :: Flammable TBlendMode where
  spark = TBlendMode <$> select "BlendMode" Multiply [Screen, Overlay] modeToString

newtype TColorSpace = TColorSpace ColorSpace

csToString RGB = "RGB"
csToString HSL = "HSL"
csToString LCh = "LCh"
csToString Lab = "Lab"

instance flammableTColorSpace :: Flammable TColorSpace where
  spark = TColorSpace <$> select "ColorSpace" HSL [RGB, LCh, Lab] csToString

newtype Int255 = Int255 Int
instance flammableInt255 :: Flammable Int255 where
  spark = Int255 <$> intSlider "Int" 0 255 100

newtype TColorScale = TColorScale ColorScale

data CSTypes = Grayscale | Spectrum | Magma | Inferno | Plasma | Viridis

instance flammableTColorScale :: Flammable TColorScale where
  spark = (TColorScale <<< toColorScale) <$> (fieldset "ColorScale" $
            select "Choose" Grayscale [Spectrum, Magma, Inferno, Plasma, Viridis] toString
          )
    where
      toString Grayscale = "grayscale"
      toString Spectrum  = "spectrum"
      toString Magma     = "magma"
      toString Inferno   = "inferno"
      toString Plasma    = "plasma"
      toString Viridis   = "viridis"

      toColorScale Grayscale = grayscale
      toColorScale Spectrum  = spectrum
      toColorScale Magma     = magma
      toColorScale Inferno   = inferno
      toColorScale Plasma    = plasma
      toColorScale Viridis   = viridis

instance interactiveTColorScale :: Interactive TColorScale where
  interactive ui = (SetHTML <<< pretty) <$> ui
    where
      pretty (TColorScale scale) = H.div ! HA.style (css scale) $ H.text ""
      css scale = "background: linear-gradient(to right, " <> cssColorStops scale <> ");" <>
                  "width: 100%; height: 30px;"

textReadable bgColor textColor = do
  H.div ! HA.style css $ H.text "Is this text well readable?"
  H.p (H.text ("WCAG says: ") <> H.b (H.text answ))
  H.div ! HA.style ("width: 400px; height: 15px; background-color: " <> barBg) $ do
    H.div ! HA.style ("width: " <> width <> "px; height: 15px;" <>
                      "background-color: " <> barFg) $ H.text ""

  where
    css = "background-color: " <> cssStringHSLA bgColor <> ";" <>
          "width: 380px; height: 50px;" <>
          "border: 1px solid black;" <>
          "padding: 10px; color: " <> cssStringHSLA textColor
    isRead = isReadable bgColor textColor
    answ = if isRead then "yes" else "no"
    ratio = contrast bgColor textColor
    width = show ((ratio - 1.0) * 20.0)
    barBg = cssStringHSLA black
    barFg = cssStringHSLA (if isRead then green else red)

flare1 = textReadable <$> color "Background" mediumvioletred
                      <*> color "Text" black

blendUI c1 c2 mode = do
  H.p (H.text "These are three separate divs (no transparency):")
  H.div ! HA.style "position: relative; height: 150px;" $ do
    (H.div ! HA.style css1) (H.text "")
    (H.div ! HA.style css2) (H.text "")
    (H.div ! HA.style css3) (H.text "")

  where
    css1 = "background-color: " <> cssStringHSLA c1 <> ";" <>
           "width: 100px; height: 100px; border: 1px solid black;" <>
           "position: absolute; top: 0px; left: 0px;"
    css2 = "background-color: " <> cssStringHSLA c2 <> ";" <>
           "width: 100px; height: 100px; border: 1px solid black;" <>
           "position: absolute; top: 50px; left: 50px;"
    css3 = "background-color: " <> cssStringHSLA (blend mode c1 c2) <> ";" <>
           "width: 50px; height: 50px; border: 1px solid black;" <>
           "position: absolute; top: 50px; left: 50px;"


flare2 = blendUI <$> color "Background" royalblue
                 <*> color "Foreground" gold
                 <*> select "BlendMode" Multiply [Screen, Overlay] modeToString

palettes c1 c2 n = H.table $ foldMap row [HSL, RGB, Lab, LCh]
    where row mode = H.tr $ (H.td $ H.text (csToString mode))
                            <> foldMap cell (colors (scale mode) n)
          cell col = H.td ! HA.style (css col) $ H.text ""
          scale mode = uniformScale mode c1 Nil c2
          css col = "width: 50px; height: 50px; background-color: " <>
                         cssStringHSLA col

flare3 = palettes <$> color "From" (fromInt 0xf5f6de)
                  <*> color "To"   (fromInt 0x1d052b)
                  <*> intSlider "Number" 4 10 6

main = do
  withPackage "purescript-colors.json" $ \dict -> do
    let doc :: forall t. Interactive t => String -> t -> _
        doc = flareDoc' "doc-color" dict "Color"

    doc "hsl" (id :: TColor -> _)
    doc "rgb" $
      (\(Int255 r) (Int255 g) (Int255 b) -> TColor (rgb r g b))
    doc "toHexString" (\(TColor c) -> toHexString c)
    doc "cssStringHSLA" (\(TColor c) -> cssStringHSLA c)
    doc "black" (TColor black)
    doc "white" (TColor white)
    doc "graytone" (\(SmallNumber s) -> TColor (graytone s))
    doc "complementary" (\(TColor c) -> ColorList [c, complementary c])
    doc "lighten"    (\(SmallNumber a) (TColor c) -> ColorList [c, lighten a c])
    doc "darken"     (\(SmallNumber a) (TColor c) -> ColorList [c, darken a c])
    doc "saturate"   (\(SmallNumber a) (TColor c) -> ColorList [c, saturate a c])
    doc "desaturate" (\(SmallNumber a) (TColor c) -> ColorList [c, desaturate a c])
    doc "mix"        (\(TColorSpace s) (TColor b) (TColor f) (SmallNumber frac) -> ColorList [b, f, mix s b f frac])
    doc "brightness" (\(TColor c) -> brightness c)
    doc "luminance"  (\(TColor c) -> luminance c)
    doc "textColor"  (\(TColor c) -> ColorList [c, textColor c])

    let docblend :: forall t. Interactive t => String -> t -> _
        docblend = flareDoc' "doc-blending" dict "Color.Blending"

    docblend "blend" (\(TBlendMode m) (TColor b) (TColor f) -> ColorList [b, f, blend m b f])

    let docscale :: forall t. Interactive t => String -> t -> _
        docscale = flareDoc' "doc-scale" dict "Color.Scale"

    docscale "colorScale" $ \(TColorSpace mode) (TColor b) (TColor e) -> TColorScale $ colorScale mode b Nil e
    docscale "addStop" $ \(TColorScale sc) (SmallNumber r) (TColor c) -> TColorScale $ addStop sc c r
    docscale "sample" $ \(TColorScale sc) (SmallNumber r) -> TColor (sample sc r)
    docscale "colors" $ \(TColorScale sc) (SmallInt n) -> ColorList (fromList $ colors sc n)
    docscale "grayscale" (TColorScale grayscale)
    docscale "spectrum" (TColorScale spectrum)
    docscale "hot" (TColorScale hot)
    docscale "cool" (TColorScale cool)
    docscale "cssColorStops" $ \(TColorSpace mode) (TColor b) (TColor e) -> cssColorStops (colorScale mode b Nil e)

    let docscaleperc :: forall t. Interactive t => String -> t -> _
        docscaleperc = flareDoc' "doc-scale-perc" dict "Color.Scale.Perceptual"

    docscaleperc "magma" $ TColorScale magma
    docscaleperc "inferno" $ TColorScale inferno
    docscaleperc "plasma" $ TColorScale plasma
    docscaleperc "viridis" $ TColorScale viridis

    let docharm :: forall t. Interactive t => String -> t -> _
        docharm = flareDoc' "doc-scheme-harm" dict "Color.Scheme.Harmonic"

    docharm "analogous" (\(TColor c) -> ColorList $ analogous c)
    docharm "triad" (\(TColor c) -> ColorList $ triad c)
    docharm "splitComplementary" (\(TColor c) -> ColorList $ splitComplementary c)
    docharm "shades" (\(TColor c) -> ColorList $ shades c)
    docharm "tetrad" (\(TColor c) -> ColorList $ tetrad c)

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

  runFlareHTML "input1" "output1" flare1
  runFlareHTML "input2" "output2" flare2
  runFlareHTML "input3" "output3" flare3
