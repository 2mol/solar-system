module Sup exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Circle2d
import Geometry.Svg as Svg
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (on, onClick)
import Json.Decode as Json
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Round
import SketchPlane3d exposing (SketchPlane3d)
import String
import Svg exposing (Svg)
import Svg.Attributes as SvgA exposing (height, width, x, y)
import Vector3d exposing (Vector3d)


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { sun : Body
    , planets : List Body
    , projection : Projection
    , runState : RunState
    , timeElapsed : Float
    , frameTime : Float
    }


type alias Body =
    { mass : EarthMass
    , position : Point3d
    , velocity : Vector3d
    , radius : Float
    , color : String
    }


type alias EarthMass =
    Float


type alias Projection =
    { plane : SketchPlane3d
    , center : Point2d
    , scale : Float
    }


type RunState
    = Running
    | Paused


type Msg
    = Nope
    | Tick Float
    | ToggleRunState
    | Zoom Int


init : ( Model, Cmd msg )
init =
    ( initModel, Cmd.none )


initModel : Model
initModel =
    { sun = initSun
    , planets = [ earth, mars, venus ]
    , projection =
        { plane = SketchPlane3d.xy
        , center = Point2d.fromCoordinates ( 0, 0 )
        , scale = 2.0e-9
        }
    , runState = Paused
    , timeElapsed = 0
    , frameTime = 0
    }


initSun : Body
initSun =
    { mass = 1.98847e6
    , position = Point3d.fromCoordinates ( 0, 0, 0 )
    , velocity = Vector3d.fromComponents ( 0, 0, 0 )
    , radius = 695508000
    , color = "#fff200"
    }


earth : Body
earth =
    { mass = 5.9722
    , position = Point3d.fromCoordinates ( 147098074000, 0, 0 )
    , velocity = Vector3d.fromComponents ( 0, -30300, 0 )
    , radius = 6371000
    , color = "#66f"
    }


mars : Body
mars =
    { mass = 0.64171
    , position = Point3d.fromCoordinates ( 206620000000, 0, 0 )
    , velocity = Vector3d.fromComponents ( 0, -26500, 0 )
    , radius = 3390000
    , color = "#991600"
    }


venus : Body
venus =
    { mass = 4.868
    , position = Point3d.fromCoordinates ( 107480000000, 0, 0 )
    , velocity = Vector3d.fromComponents ( 0, -35500, 0 )
    , radius = 6052000
    , color = "#a3159c"
    }


update : Msg -> Model -> ( Model, Cmd msg )
update msg ({ runState, sun, planets, projection } as model) =
    case msg of
        Nope ->
            ( model, Cmd.none )

        Tick dt ->
            let
                repeatedEuler planet =
                    applyN 2000 (euler 20 sun) planet

                planets_ =
                    List.map repeatedEuler planets
            in
            ( { model | planets = planets_, frameTime = dt }, Cmd.none )

        ToggleRunState ->
            ( { model | runState = toggleRunState runState }, Cmd.none )

        Zoom i ->
            let
                i_ =
                    toFloat i |> negate

                scale_ =
                    (projection.scale + (i_ * projection.scale * 1.0e-3))
                        |> max 0

                projection_ =
                    { projection | scale = scale_ }
            in
            ( { model | projection = projection_ }, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div [ style "display" "flex" ]
        [ Html.div paneStyle
            [ controlPane model ]
        , Html.div [ style "flex" "1" ]
            [ drawing model ]
        ]


subscriptions : Model -> Sub Msg
subscriptions { runState } =
    if runState == Paused then
        Sub.none

    else
        onAnimationFrameDelta Tick



-- subviews


controlPane : Model -> Html Msg
controlPane { sun, planets, frameTime, projection } =
    Html.div []
        [ Html.button [ onClick ToggleRunState ] [ Html.text "play/pause" ]
        , Html.table
            [ style "border" "1px solid black"
            , style "width" "100%"
            , style "table-layout" "fixed"
            ]
            [ Html.tr []
                [ Html.td tdStyleLeft [ Html.text "fps" ]
                , Html.td tdStyleRight [ Html.text <| Round.round 2 (1000 / frameTime) ]
                ]
            , Html.tr []
                [ Html.td tdStyleLeft [ Html.text "scale" ]
                , Html.td tdStyleRight [ Html.text <| Round.round 10 projection.scale ]
                ]
            ]
        ]


paneStyle =
    [ style "flex" "1"
    , style "padding" "1rem"
    , style "font-family" "Courier New"
    , style "font-size" "0.6em"
    ]


tdStyleLeft =
    [ style "width" "80px", style "word-wrap" "break-word" ]


tdStyleRight =
    [ style "width" "120px", style "word-wrap" "break-word" ]


toggleRunState : RunState -> RunState
toggleRunState runState =
    case runState of
        Paused ->
            Running

        Running ->
            Paused


onWheel : (Int -> msg) -> Html.Attribute msg
onWheel message =
    on "wheel" (Json.map message (Json.at [ "deltaY" ] Json.int))



-- calculations


constMassScale : Float
constMassScale =
    1.0e24


constG : Float
constG =
    6.67408e-11


constGmod : Float
constGmod =
    constG * constMassScale


applyN : Int -> (a -> a) -> a -> a
applyN n fun input =
    if n <= 0 then
        input

    else
        applyN (n - 1) fun (fun input)


euler : Float -> Body -> Body -> Body
euler dt fixedBody movingBody =
    let
        ( x, y, z ) =
            Point3d.coordinates movingBody.position

        ( u, v, w ) =
            Vector3d.components movingBody.velocity

        quot =
            dt * constGmod * fixedBody.mass / (x ^ 2 + y ^ 2 + z ^ 2) ^ (3 / 2)

        ( xn, yn, zn ) =
            ( x + u * dt
            , y + v * dt
            , z + w * dt
            )

        ( un, vn, wn ) =
            ( u - x * quot
            , v - y * quot
            , w - z * quot
            )
    in
    { movingBody
        | position = Point3d.fromCoordinates ( xn, yn, zn )
        , velocity = Vector3d.fromComponents ( un, vn, wn )
    }



--


drawing : Model -> Html Msg
drawing { sun, planets, projection } =
    let
        sketchPlane =
            SketchPlane3d.xy

        bodies =
            drawBody projection sun :: List.map (drawBody projection) planets

        background =
            Svg.rect [ x "-500", y "-300", width "100%", height "100%", SvgA.fill "#f7f7f7" ] []
    in
    Svg.svg
        [ width "1000", height "600", SvgA.viewBox "-500 -300 1000 600", onWheel Zoom ]
        (background :: bodies)



--


drawBody : Projection -> Body -> Svg msg
drawBody { plane, center, scale } { mass, position, radius, color } =
    let
        scaledRadius =
            radius * scale

        scaledPosition =
            position
                |> Point3d.projectInto plane
                |> Point2d.scaleAbout center scale

        atmocircle =
            [ Svg.circle2d [ SvgA.fill color, SvgA.opacity "0.15" ]
                (Circle2d.withRadius (scaledRadius + 15) scaledPosition)
            ]
    in
    Svg.g []
        (atmocircle
            ++ [ Svg.circle2d [ SvgA.fill color ]
                    (Circle2d.withRadius scaledRadius scaledPosition)
               ]
        )
