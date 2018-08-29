module Plotting exposing (Plot, Range(..), addDataPoint, draw, new, setMaxPoints, setRange)

import Array as A exposing (Array)
import Html exposing (Html)
import Html.Attributes as HtmlA exposing (style)
import Maybe
import Round
import Svg exposing (Svg)
import Svg.Attributes as S


type Plot a
    = Plot
        { name : String
        , dataPoints : Array a
        , nPoints : Int
        , range : Range
        }


new : String -> Plot a
new name =
    Plot { name = name, dataPoints = A.empty, nPoints = 500, range = Dynamic }


setMaxPoints : Int -> Plot a -> Plot a
setMaxPoints n (Plot plot) =
    Plot { plot | nPoints = n }


setRange : Range -> Plot a -> Plot a
setRange r (Plot plot) =
    Plot { plot | range = r }


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
    | Dynamic


draw : ( Int, Int ) -> String -> Plot Float -> Html msg
draw ( width, height ) color (Plot p) =
    let
        dx =
            toFloat width / toFloat (p.nPoints - 1)

        ( yMin, yMax ) =
            case p.range of
                Dynamic ->
                    ( Maybe.withDefault 0 <| List.minimum <| A.toList p.dataPoints
                    , Maybe.withDefault 100 <| List.maximum <| A.toList p.dataPoints
                    )

                Fixed ( y1, y2 ) ->
                    ( y1, y2 )

        range =
            yMax - yMin

        scale y =
            (y - yMin) / range * toFloat height

        coordPairString i y =
            String.join
                ","
                [ String.fromFloat (toFloat i * dx)
                , String.fromFloat (toFloat height - y)
                ]

        coordString =
            p.dataPoints
                |> A.map scale
                |> A.indexedMap coordPairString
                |> A.toList
                |> String.join " "

        wstr =
            String.fromInt width

        hstr =
            String.fromInt height

        hstrHalf =
            toFloat height
                / 2
                |> round
                |> String.fromInt

        avg =
            average p.dataPoints

        hstrAvg =
            toFloat height - (scale avg)
                |> String.fromFloat

        hAvgLabel =
            (toFloat height - scale avg) - 3
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
        [ Svg.text_ [ S.x "0.3em", S.y "1em" ] [ Svg.text p.name ]
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
    A.foldl (+) 0 arr / (toFloat <| A.length arr)
