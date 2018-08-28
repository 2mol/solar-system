module Plotting exposing (Range(..), TPlot(..), tplot, tplotHelper)

import Html.Attributes exposing (style)
import Svg exposing (Svg)
import Svg.Attributes as SvgA


type TPlot
    = TPlot
        { dataPoints : List Float
        , maxPoints : Int
        , range : Range
        }


type Range
    = Fixed ( Float, Float )
    | Dynamic -- adapts to max and min, making the viewport fit the plot
    | DynamicSym -- reacts to max and min values, but keeping the median in the center -> Symmetric


tplot ys =
    let
        width =
            1000

        maxPoints : Int
        maxPoints =
            300

        dx =
            width / toFloat maxPoints

        coordStrings =
            List.indexedMap (\i y -> String.fromFloat (width - toFloat i * dx) ++ "," ++ String.fromFloat y) ys

        coordString =
            String.join " " coordStrings
    in
    Svg.svg
        [ SvgA.width "1000", SvgA.height "200", SvgA.viewBox "0 0 1000 200", style "border" "1px solid black" ]
        [ Svg.line [ SvgA.x1 "0", SvgA.y1 "100", SvgA.x2 "1000", SvgA.y2 "100", SvgA.stroke "black" ] []

        -- , Svg.polyline [ SvgA.fill "none", SvgA.stroke "pink", SvgA.points "20,100 40,60 70,80 100,20" ] []
        , Svg.polyline [ SvgA.fill "none", SvgA.stroke "pink", SvgA.points coordString ] []
        ]


tplotHelper ys =
    let
        s =
            \i y -> String.fromInt i ++ "," ++ String.fromInt (round y)

        bla =
            List.indexedMap
    in
    bla
