module Main (main) where

import Prelude

import Data.Array (sortBy, take, fromFoldable)
import Data.Foldable (foldMap)
import Data.List (List(..), (:))
import Data.Ord (comparing)
import Data.Tuple (Tuple (..))
import Data.NonEmpty ((:|))

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
import Color.Scheme.Clrs as Clrs

newtype TColor = TColor Color

runTColor :: TColor -> Color
runTColor (TColor c) = c

instance flammableTColor :: Flammable TColor where
  examples = TColor <$> (hsl 231.0 0.48 0.38  :| MD.blue : MD.orange : Nil)

  spark (TColor c) = TColor <$> fieldset "Color"
                           (hsl <$> numberSlider "Hue" 0.0 360.0 0.1 col.h
                                <*> numberSlider "Saturation" 0.0 1.0 0.001 col.s
                                <*> numberSlider "Lightness" 0.0 1.0 0.001 col.l)
    where col = toHSLA c

colorBox :: Color -> H.Markup Unit
colorBox c = H.div ! HA.style css $ H.code (H.text repr)
  where
    repr = cssStringHSLA c
    css = "background-color: " <> repr <> ";" <>
          "width: 260px; height: 50px; display: inline-block; font-size: 13px;" <>
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
  examples = TBlendMode <$> (Multiply :| Screen : Overlay : Nil)
  spark _ = TBlendMode <$> select "BlendMode" (Multiply :| [Screen, Overlay]) modeToString

newtype TColorSpace = TColorSpace ColorSpace

csToString RGB = "RGB"
csToString HSL = "HSL"
csToString LCh = "LCh"
csToString Lab = "Lab"

instance flammableTColorSpace :: Flammable TColorSpace where
  examples = TColorSpace <$> (HSL :| RGB : LCh : Lab : Nil)
  spark _ = TColorSpace <$> select "ColorSpace" (HSL :| [RGB, LCh, Lab]) csToString

newtype Int255 = Int255 Int
instance flammableInt255 :: Flammable Int255 where
  examples = Int255 <$> (127 :| 50 : 200 : 0 : Nil)
  spark (Int255 def) = Int255 <$> intSlider "Int" 0 255 def

newtype TColorScale = TColorScale ColorScale

data CSTypes
  = Grayscale
  | Spectrum
  | SpectrumLCh
  | BlueToRed
  | YellowToRed
  | Hot
  | Cool
  | Magma
  | Inferno
  | Plasma
  | Viridis

toString Grayscale   = "grayscale"
toString Spectrum    = "spectrum"
toString SpectrumLCh = "spectrumLCh"
toString BlueToRed   = "blueToRed"
toString YellowToRed = "yellowToRed"
toString Hot         = "hot"
toString Cool        = "cool"
toString Magma       = "magma"
toString Inferno     = "inferno"
toString Plasma      = "plasma"
toString Viridis     = "viridis"

toColorScale Grayscale   = grayscale
toColorScale Spectrum    = spectrum
toColorScale SpectrumLCh = spectrumLCh
toColorScale BlueToRed   = blueToRed
toColorScale YellowToRed = yellowToRed
toColorScale Hot         = hot
toColorScale Cool        = cool
toColorScale Magma       = magma
toColorScale Inferno     = inferno
toColorScale Plasma      = plasma
toColorScale Viridis     = viridis

instance flammableTColorScale :: Flammable TColorScale where
  examples = (TColorScale <<< toColorScale) <$> (Spectrum :| SpectrumLCh : BlueToRed : Nil)
  spark _ = (TColorScale <<< toColorScale) <$> (fieldset "ColorScale" $
              select "Choose" (Grayscale :| [Spectrum, SpectrumLCh, BlueToRed,
                                             YellowToRed, Hot, Cool, Magma, Inferno,
                                             Plasma, Viridis]) toString
            )

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
                 <*> select "BlendMode" (Multiply :| [Screen, Overlay]) modeToString

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

mapX11 =
  [ Tuple "black" black
  , Tuple "white" white
  , Tuple "aliceblue" aliceblue
  , Tuple "antiquewhite" antiquewhite
  , Tuple "aqua" aqua
  , Tuple "aquamarine" aquamarine
  , Tuple "azure" azure
  , Tuple "beige" beige
  , Tuple "bisque" bisque
  , Tuple "blanchedalmond" blanchedalmond
  , Tuple "blue" blue
  , Tuple "blueviolet" blueviolet
  , Tuple "brown" brown
  , Tuple "burlywood" burlywood
  , Tuple "cadetblue" cadetblue
  , Tuple "chartreuse" chartreuse
  , Tuple "chocolate" chocolate
  , Tuple "coral" coral
  , Tuple "cornflowerblue" cornflowerblue
  , Tuple "cornsilk" cornsilk
  , Tuple "crimson" crimson
  , Tuple "cyan" cyan
  , Tuple "darkblue" darkblue
  , Tuple "darkcyan" darkcyan
  , Tuple "darkgoldenrod" darkgoldenrod
  , Tuple "darkgray" darkgray
  , Tuple "darkgreen" darkgreen
  , Tuple "darkgrey" darkgrey
  , Tuple "darkkhaki" darkkhaki
  , Tuple "darkmagenta" darkmagenta
  , Tuple "darkolivegreen" darkolivegreen
  , Tuple "darkorange" darkorange
  , Tuple "darkorchid" darkorchid
  , Tuple "darkred" darkred
  , Tuple "darksalmon" darksalmon
  , Tuple "darkseagreen" darkseagreen
  , Tuple "darkslateblue" darkslateblue
  , Tuple "darkslategray" darkslategray
  , Tuple "darkslategrey" darkslategrey
  , Tuple "darkturquoise" darkturquoise
  , Tuple "darkviolet" darkviolet
  , Tuple "deeppink" deeppink
  , Tuple "deepskyblue" deepskyblue
  , Tuple "dimgray" dimgray
  , Tuple "dimgrey" dimgrey
  , Tuple "dodgerblue" dodgerblue
  , Tuple "firebrick" firebrick
  , Tuple "floralwhite" floralwhite
  , Tuple "forestgreen" forestgreen
  , Tuple "fuchsia" fuchsia
  , Tuple "gainsboro" gainsboro
  , Tuple "ghostwhite" ghostwhite
  , Tuple "gold" gold
  , Tuple "goldenrod" goldenrod
  , Tuple "gray" gray
  , Tuple "green" green
  , Tuple "greenyellow" greenyellow
  , Tuple "grey" grey
  , Tuple "honeydew" honeydew
  , Tuple "hotpink" hotpink
  , Tuple "indianred" indianred
  , Tuple "indigo" indigo
  , Tuple "ivory" ivory
  , Tuple "khaki" khaki
  , Tuple "lavender" lavender
  , Tuple "lavenderblush" lavenderblush
  , Tuple "lawngreen" lawngreen
  , Tuple "lemonchiffon" lemonchiffon
  , Tuple "lightblue" lightblue
  , Tuple "lightcoral" lightcoral
  , Tuple "lightcyan" lightcyan
  , Tuple "lightgoldenrodyellow" lightgoldenrodyellow
  , Tuple "lightgray" lightgray
  , Tuple "lightgreen" lightgreen
  , Tuple "lightgrey" lightgrey
  , Tuple "lightpink" lightpink
  , Tuple "lightsalmon" lightsalmon
  , Tuple "lightseagreen" lightseagreen
  , Tuple "lightskyblue" lightskyblue
  , Tuple "lightslategray" lightslategray
  , Tuple "lightslategrey" lightslategrey
  , Tuple "lightsteelblue" lightsteelblue
  , Tuple "lightyellow" lightyellow
  , Tuple "lime" lime
  , Tuple "limegreen" limegreen
  , Tuple "linen" linen
  , Tuple "magenta" magenta
  , Tuple "maroon" maroon
  , Tuple "mediumaquamarine" mediumaquamarine
  , Tuple "mediumblue" mediumblue
  , Tuple "mediumorchid" mediumorchid
  , Tuple "mediumpurple" mediumpurple
  , Tuple "mediumseagreen" mediumseagreen
  , Tuple "mediumslateblue" mediumslateblue
  , Tuple "mediumspringgreen" mediumspringgreen
  , Tuple "mediumturquoise" mediumturquoise
  , Tuple "mediumvioletred" mediumvioletred
  , Tuple "midnightblue" midnightblue
  , Tuple "mintcream" mintcream
  , Tuple "mistyrose" mistyrose
  , Tuple "moccasin" moccasin
  , Tuple "navajowhite" navajowhite
  , Tuple "navy" navy
  , Tuple "oldlace" oldlace
  , Tuple "olive" olive
  , Tuple "olivedrab" olivedrab
  , Tuple "orange" orange
  , Tuple "orangered" orangered
  , Tuple "orchid" orchid
  , Tuple "palegoldenrod" palegoldenrod
  , Tuple "palegreen" palegreen
  , Tuple "paleturquoise" paleturquoise
  , Tuple "palevioletred" palevioletred
  , Tuple "papayawhip" papayawhip
  , Tuple "peachpuff" peachpuff
  , Tuple "peru" peru
  , Tuple "pink" pink
  , Tuple "plum" plum
  , Tuple "powderblue" powderblue
  , Tuple "purple" purple
  , Tuple "red" red
  , Tuple "rosybrown" rosybrown
  , Tuple "royalblue" royalblue
  , Tuple "saddlebrown" saddlebrown
  , Tuple "salmon" salmon
  , Tuple "sandybrown" sandybrown
  , Tuple "seagreen" seagreen
  , Tuple "seashell" seashell
  , Tuple "sienna" sienna
  , Tuple "silver" silver
  , Tuple "skyblue" skyblue
  , Tuple "slateblue" slateblue
  , Tuple "slategray" slategray
  , Tuple "slategrey" slategrey
  , Tuple "snow" snow
  , Tuple "springgreen" springgreen
  , Tuple "steelblue" steelblue
  , Tuple "tan" tan
  , Tuple "teal" teal
  , Tuple "thistle" thistle
  , Tuple "tomato" tomato
  , Tuple "turquoise" turquoise
  , Tuple "violet" violet
  , Tuple "wheat" wheat
  , Tuple "whitesmoke" whitesmoke
  , Tuple "yellow" yellow
  , Tuple "yellowgreen" yellowgreen
  ]

nameColor color = H.div $ foldMap toCell closest
  where
    distC (Tuple name col) = distance color col
    closest = take 3 $ sortBy (comparing distC) mapX11

    toCell (Tuple name c) = H.div ! HA.style css $ H.code (H.text name)
      where
        css = "background-color: " <> cssStringHSLA c <> ";" <>
              "width: 260px; height: 50px; display: inline-block; font-size: 13px;" <>
              "margin-top: 10px; margin-right: 10px; border: 1px solid black;" <>
              "padding: 5px; color: " <> cssStringHSLA (textColor c)

flare4 = nameColor <$> color "Input color" salmon

cubehelix' :: Color -> Color -> Number -> String
cubehelix' b e gamma = cssColorStopsRGB $ minColorStops 100 gen (ColorStops b Nil e)
  where
    gen _ = mixCubehelix gamma b e

colorScaleDiv stops = H.div ! HA.style css $ H.text ""
  where
    css = "background: linear-gradient(to right, " <> stops <> ");" <>
          "width: 100%; height: 30px;"

flare5 = colorScaleDiv <$> (cubehelix' <$> hslPick "First color" 360.0 0.0
                                       <*> hslPick "Last color" (-240.0) 1.0
                                       <*> numberSlider "Gamma (0.5 .. 3.0)" 0.5 3.0 0.1 1.0)
  where
    hslPick :: forall e. String -> Number -> Number -> UI e Color
    hslPick title h l = fieldset title $
                          hsl <$> numberSlider "Hue (-360° .. 720°)" (-360.0) 720.0 0.1 h
                              <*> numberSlider "Saturation" 0.0 1.0 0.01 1.0
                              <*> pure l

main = do
  withPackage "purescript-colors.json" $ \dict -> do
    let doc :: forall t. Interactive t => String -> t -> _
        doc = flareDoc' "doc-color" dict "Color"

    doc "hsl" (id :: TColor -> _)
    doc "rgb" $
      (\(Int255 r) (Int255 g) (Int255 b) -> TColor (rgb r g b))
    doc "toHexString" toHexString
    doc "cssStringHSLA" cssStringHSLA
    doc "black" (TColor black)
    doc "white" (TColor white)
    doc "graytone" (\(SmallNumber s) -> TColor (graytone s))
    doc "complementary" complementary
    doc "lighten"    (\(SmallNumber a) -> TColor <<< lighten a)
    doc "darken"     (\(SmallNumber a) -> TColor <<< darken a)
    doc "saturate"   (\(SmallNumber a) -> TColor <<< saturate a)
    doc "desaturate" (\(SmallNumber a) -> TColor <<< desaturate a)
    doc "mix"        (\(TColorSpace s) b f (SmallNumber frac) -> TColor (mix s b f frac))
    doc "mixCubehelix" (\gamma b f (SmallNumber frac) -> TColor (mixCubehelix gamma b f frac))
    doc "brightness" brightness
    doc "luminance"  luminance
    doc "textColor"  (TColor <<< textColor)

    let docblend :: forall t. Interactive t => String -> t -> _
        docblend = flareDoc' "doc-blending" dict "Color.Blending"

    docblend "blend" (\(TBlendMode m) b f -> TColor (blend m b f))

    let docscale :: forall t. Interactive t => String -> t -> _
        docscale = flareDoc' "doc-scale" dict "Color.Scale"

    docscale "colorScale" $ \(TColorSpace mode) b e -> TColorScale $ colorScale mode b Nil e
    docscale "addStop" $ \(TColorScale sc) (SmallNumber r) c -> TColorScale $ addStop sc c r
    docscale "sample" $ \(TColorScale sc) (SmallNumber r) -> TColor (sample sc r)
    docscale "colors" $ \(TColorScale sc) (SmallInt n) -> ColorList (fromFoldable $ colors sc n)
    docscale "grayscale" (TColorScale grayscale)
    docscale "spectrum" (TColorScale spectrum)
    docscale "spectrumLCh" (TColorScale spectrumLCh)
    docscale "blueToRed" (TColorScale blueToRed)
    docscale "yellowToRed" (TColorScale yellowToRed)
    docscale "hot" (TColorScale hot)
    docscale "cool" (TColorScale cool)
    docscale "cubehelix" (TColorScale cubehelix)
    docscale "cssColorStops" $ \(TColorSpace mode) b e -> cssColorStops (colorScale mode b Nil e)
    docscale "modify (here: modify (const toGray))" $ \(TColorScale sc) -> TColorScale (modify (const toGray) sc)

    let docscaleperc :: forall t. Interactive t => String -> t -> _
        docscaleperc = flareDoc' "doc-scale-perc" dict "Color.Scale.Perceptual"

    docscaleperc "magma" $ TColorScale magma
    docscaleperc "inferno" $ TColorScale inferno
    docscaleperc "plasma" $ TColorScale plasma
    docscaleperc "viridis" $ TColorScale viridis

    let docharm :: forall t. Interactive t => String -> t -> _
        docharm = flareDoc' "doc-scheme-harm" dict "Color.Scheme.Harmonic"

    docharm "analogous" (\c -> ColorList $ analogous c)
    docharm "triad" (\c -> ColorList $ triad c)
    docharm "splitComplementary" (\c -> ColorList $ splitComplementary c)
    docharm "shades" (\c -> ColorList $ shades c)
    docharm "tetrad" (\c -> ColorList $ tetrad c)

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

    let docClrs :: forall t. Interactive t => String -> t -> _
        docClrs = flareDoc' "doc-scheme-clrs" dict "Color.Scheme.Clrs"

    docClrs "navy" (TColor Clrs.navy)
    docClrs "blue" (TColor Clrs.blue)
    docClrs "aqua" (TColor Clrs.aqua)
    docClrs "teal" (TColor Clrs.teal)
    docClrs "olive" (TColor Clrs.olive)
    docClrs "green" (TColor Clrs.green)
    docClrs "lime" (TColor Clrs.lime)
    docClrs "yellow" (TColor Clrs.yellow)
    docClrs "orange" (TColor Clrs.orange)
    docClrs "red" (TColor Clrs.red)
    docClrs "maroon" (TColor Clrs.maroon)
    docClrs "fuchsia" (TColor Clrs.fuchsia)
    docClrs "purple" (TColor Clrs.purple)
    docClrs "black'" (TColor Clrs.black')
    docClrs "gray" (TColor Clrs.gray)
    docClrs "silver" (TColor Clrs.silver)

  runFlareHTML "input1" "output1" flare1
  runFlareHTML "input2" "output2" flare2
  runFlareHTML "input3" "output3" flare3
  runFlareHTML "input4" "output4" flare4
  runFlareHTML "input5" "output5" flare5
