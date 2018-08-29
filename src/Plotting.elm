module Plotting exposing (Plot, Range(..), addDataPoint, draw, new)

import Array as A exposing (Array)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Maybe
import Svg exposing (Svg)
import Svg.Attributes as S
import Round


type Plot a
    = Plot
        { name : String
        , dataPoints : Array a
        , nPoints : Int
        }


new : Int -> String -> Plot a
new n name =
    Plot { name = name, dataPoints = A.empty, nPoints = n }


addDataPoint : Plot a -> a -> Plot a
addDataPoint (Plot plot) x =
    let
        newDataPoints =
            if A.length plot.dataPoints < plot.nPoints then
                A.push x plot.dataPoints

            else
                A.push x plot.dataPoints |> A.slice 1 (plot.nPoints + 1)
    in
    Plot { plot | dataPoints = newDataPoints }


replaceData : Plot a -> List a -> Plot a
replaceData (Plot plot) xs =
    Plot { plot | dataPoints = A.slice 0 plot.nPoints <| A.fromList xs }


type Range
    = Fixed ( Float, Float )
    | Dynamic -- adapts to max and min, making the viewport fit the plot
    | DynamicSym -- reacts to max and min values, but keeping the median in the center -> Symmetric


draw : ( Int, Int ) -> String -> Plot Float -> Html msg
draw ( width, height ) color (Plot plot) =
    let
        dx =
            toFloat width / toFloat (plot.nPoints - 1)

        coordPairString i y =
            String.join
                ","
                [ String.fromFloat (toFloat i * dx)
                , String.fromFloat (toFloat height - y)
                ]

        coordString =
            plot.dataPoints
                |> A.indexedMap coordPairString
                |> A.toList
                |> String.join " "

        wstr =
            String.fromInt width

        hstr =
            String.fromInt height

        hstrHalf =
            toFloat height / 2
                |> round
                |> String.fromInt

        avg =
            average plot.dataPoints

        hstrAvg =
            toFloat height - avg
                |> String.fromFloat

        hAvgLabel =
            toFloat height - avg - 3
                |> String.fromFloat

        viewBox =
            String.join " " [ "0", "0", wstr, hstr ]
    in
    Svg.svg
        [ S.width wstr
        , S.height hstr
        , S.viewBox viewBox
        , style "border" "1px solid black"
        , S.fontFamily "Courier New"
        , S.fontSize "12"
        ]
        [ Svg.text_ [ S.x "0.3em", S.y "1em" ] [ Svg.text plot.name ]
        , Svg.line [ S.x1 "0", S.y1 hstrHalf, S.x2 wstr, S.y2 hstrHalf, S.stroke "black" ] []
        , Svg.polyline [ S.fill "none", S.stroke color, S.points coordString ] []
        , Svg.text_ [ S.x (String.fromInt <| width - 30), S.y hAvgLabel ] [ Svg.text (Round.round 1 avg) ]
        , Svg.line [ S.x1 "0", S.y1 hstrAvg, S.x2 wstr, S.y2 hstrAvg, S.stroke "#bbb" ] []
        ]


median : Array Float -> Float
median arr =
    let
        m =
            round <| toFloat (A.length arr) / 2
    in
    A.toList arr
        |> List.sort
        |> A.fromList
        |> A.get m
        |> Maybe.withDefault 0

average : Array Float -> Float
average arr =
    (A.foldl (+) 0 arr) / (toFloat <| A.length arr)
