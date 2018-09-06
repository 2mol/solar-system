module TinyPlot exposing
    ( Plot
    , XRange(..)
    , YRange(..)
    , draw
    , new
    , pushData
    , replaceData
    , setMaxPoints
    , setXRange
    , setYRange
    )

import Array as A exposing (Array)
import Html exposing (Html)
import Html.Attributes as HtmlA exposing (style)
import Maybe
import Svg exposing (Svg)
import Svg.Attributes as S


type Plot a
    = Plot
        { name : String
        , dataPoints : Array a
        , nPoints : Int
        , yRange : YRange
        , xRange : XRange
        }


new : String -> Plot a
new name =
    Plot { name = name, dataPoints = A.empty, nPoints = 500, yRange = Dynamic, xRange = Sliding }


setMaxPoints : Int -> Plot a -> Plot a
setMaxPoints n (Plot plot) =
    Plot { plot | nPoints = n }


setYRange : YRange -> Plot a -> Plot a
setYRange r (Plot plot) =
    Plot { plot | yRange = r }


setXRange : XRange -> Plot a -> Plot a
setXRange r (Plot plot) =
    Plot { plot | xRange = r }


pushData : Plot a -> a -> Plot a
pushData (Plot plot) x =
    let
        newDataPoints =
            case plot.xRange of
                Sliding ->
                    if A.length plot.dataPoints < plot.nPoints then
                        A.push x plot.dataPoints

                    else
                        A.push x plot.dataPoints |> A.slice 1 (plot.nPoints + 1)

                Accumulative ->
                    A.push x plot.dataPoints
    in
    Plot { plot | dataPoints = newDataPoints }


replaceData : Plot a -> List a -> Plot a
replaceData (Plot plot) xs =
    Plot { plot | dataPoints = A.slice 0 plot.nPoints <| A.fromList xs }


type YRange
    = Fixed ( Float, Float )
    | Dynamic


type XRange
    = Sliding
    | Accumulative


draw : ( Int, Int ) -> String -> Plot Float -> Html msg
draw ( width, height ) color (Plot p) =
    let
        dx =
            case p.xRange of
                Sliding ->
                    toFloat width / toFloat (p.nPoints - 1)

                Accumulative ->
                    toFloat width / toFloat (A.length p.dataPoints - 1)

        ( yMin, yMax ) =
            case p.yRange of
                Dynamic ->
                    ( Maybe.withDefault 0 <| List.minimum <| A.toList p.dataPoints
                    , Maybe.withDefault 0 <| List.maximum <| A.toList p.dataPoints
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

        viewBox =
            String.join " " [ "0", "0", wstr, hstr ]

        hstrHalf =
            toFloat height
                / 2
                |> round
                |> String.fromInt

        avg =
            average p.dataPoints

        hstrAvg =
            toFloat height
                - scale avg
                |> String.fromFloat

        hAvgLabel =
            (toFloat height - scale avg)
                - 3
                |> String.fromFloat

        -- avgString =
        --Round.round 1 avg
        rightAlignPos =
            String.fromInt (width - 2)

        yMinString =
            String.fromFloat yMin

        yMaxString =
            String.fromFloat yMax

        bottomPos =
            String.fromInt <| height - 4

        -- avgLine =
        --     -- TODO: make this a setting, don't link to y range
        --     case p.yRange of
        --         Fixed _ ->
        --             Svg.g []
        --                 [ Svg.text_
        --                     [ S.x rightAlignPos, S.textAnchor "end", S.y hAvgLabel ]
        --                     [ Svg.text avgString ]
        --                 , Svg.line
        --                     [ S.x1 "0", S.y1 hstrAvg, S.x2 wstr, S.y2 hstrAvg, S.stroke "black", S.opacity "0.35" ]
        --                     []
        --                 ]
        --         _ ->
        --             Svg.g [] []
    in
    Svg.svg
        [ S.width wstr
        , S.height hstr
        , S.viewBox viewBox
        , style "border" "1px solid black"
        , S.fontFamily "Courier New"
        , S.fontSize "12"
        ]
        [ Svg.line [ S.x1 "0", S.y1 hstrHalf, S.x2 wstr, S.y2 hstrHalf, S.stroke "black", S.opacity "0.25" ] []
        , Svg.polyline [ S.fill "none", S.stroke color, S.points coordString ] []

        -- , avgLine
        , Svg.text_ [ S.x "0.3em", S.y "1em" ] [ Svg.text p.name ]
        , Svg.text_ [ S.x rightAlignPos, S.textAnchor "end", S.y "1em" ] [ Svg.text yMaxString ]
        , Svg.text_ [ S.x rightAlignPos, S.textAnchor "end", S.y bottomPos ] [ Svg.text yMinString ]
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
