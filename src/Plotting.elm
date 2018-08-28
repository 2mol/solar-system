module Plotting exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (style)
import Svg exposing (Svg)
import Svg.Attributes as S


type alias Plot a =
    { dataPoints : List a
    , maxPoints : Int
    }


addDatum : Plot a -> a -> Plot a
addDatum ({ dataPoints, maxPoints } as plot) x =
    { plot | dataPoints = List.take maxPoints <| x :: dataPoints }


type Range
    = Fixed ( Float, Float )
    | Dynamic -- adapts to max and min, making the viewport fit the plot
    | DynamicSym -- reacts to max and min values, but keeping the median in the center -> Symmetric


draw : ( Int, Int ) -> Plot Float -> Html msg
draw ( width, height ) { dataPoints, maxPoints } =
    let
        dx =
            toFloat width / toFloat maxPoints

        coordString =
            dataPoints
                |> List.indexedMap (\i y -> String.join "," [ String.fromFloat (toFloat width - toFloat i * dx), String.fromFloat y ])
                |> String.join " "

        wstr =
            String.fromInt width

        hstr =
            String.fromInt height

        hstrHalf =
            toFloat height / 2
                |> round
                |> String.fromInt

        viewBox =
            String.join " " [ "0", "0", wstr, hstr ]
    in
    Svg.svg
        [ S.width wstr, S.height hstr, S.viewBox viewBox, style "border" "1px solid black" ]
        [ Svg.line
            [ S.x1 "0", S.y1 hstrHalf, S.x2 wstr, S.y2 hstrHalf, S.stroke "black" ]
            []
        , Svg.polyline [ S.fill "none", S.stroke "pink", S.points coordString ] []
        ]
