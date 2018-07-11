module Sup exposing (main)

import AnimationFrame
import Circle2d
import Geometry.Svg as Svg
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events exposing (on, onClick)
import Json.Decode as Json
import Plane3d
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Round
import SketchPlane3d exposing (SketchPlane3d)
import Svg exposing (Svg)
import Svg.Attributes as SvgA exposing (height, width, x, y)
import Vector3d exposing (Vector3d)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { earth : Body
    , moon : Body
    , trail : List Point3d
    , runState : RunState
    , projection : Projection
    , dt : Float
    }


type alias Body =
    { mass : Mass
    , position : Point3d
    , velocity : Vector3d
    , radius : Float
    , atmosphere : Float
    }


type alias Mass =
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
    | Perturb Perturbation
    | Zoom Int


init : ( Model, Cmd msg )
init =
    ( initModel, Cmd.none )


initModel : Model
initModel =
    { earth = initEarth
    , moon = initMoon
    , trail = []
    , projection =
        { plane = SketchPlane3d.xy
        , center = Point2d.fromCoordinates ( 0, 0 )
        , scale = 7.0e-7
        }
    , runState = Paused
    , dt = 0
    }


initEarth : Body
initEarth =
    { mass = 5.9722e24
    , position = Point3d.fromCoordinates ( 0, 0, 0 )
    , velocity = Vector3d.fromComponents ( 0, 0, 0 )
    , radius = 6371000
    , atmosphere = 0.05
    }


initMoon : Body
initMoon =
    { mass = 7.34767309e22
    , position = Point3d.fromCoordinates ( 384400000, 0, 0 )
    , velocity = Vector3d.fromComponents ( 0, 1000, 0 )
    , radius = 1737000
    , atmosphere = 0
    }


update : Msg -> Model -> ( Model, Cmd msg )
update msg ({ runState, earth, moon, trail, projection } as model) =
    case msg of
        Nope ->
            ( model, Cmd.none )

        Tick dt ->
            let
                trail_ =
                    List.take 450 <| moon.position :: trail

                moon_ =
                    applyN 2000 (\m -> euler 3 ( earth, m )) moon
            in
            ( { model | moon = moon_, trail = trail_, dt = dt }, Cmd.none )

        ToggleRunState ->
            ( { model | runState = toggleRunState runState }, Cmd.none )

        Perturb pert ->
            ( { model | moon = annoy pert moon }, Cmd.none )

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
    Html.div [ HtmlA.style [ ( "display", "flex" ) ] ]
        [ Html.div [ paneStyle ]
            [ controlPane model ]
        , Html.div [ HtmlA.style [ ( "flex", "1" ) ] ]
            [ drawing model ]
        ]


subscriptions : Model -> Sub Msg
subscriptions { runState } =
    if runState == Paused then
        Sub.none
    else
        AnimationFrame.diffs Tick



--


type Perturbation
    = Brake
    | Faster
    | Bother


annoy perturbation thing =
    case perturbation of
        Bother ->
            let
                velocity_ =
                    thing.velocity
                        |> Vector3d.sum (Vector3d.fromComponents ( 70, -50, 20 ))
            in
            { thing | velocity = velocity_ }

        Brake ->
            { thing | velocity = Vector3d.scaleBy 0.8 thing.velocity }

        Faster ->
            { thing | velocity = Vector3d.scaleBy 1.2 thing.velocity }



-- subviews


controlPane : Model -> Html Msg
controlPane { earth, moon, dt, projection } =
    let
        kinetic =
            kineticEnergy moon

        potential =
            potentialEnergy moon earth
    in
    Html.div []
        [ Html.button [ onClick ToggleRunState ] [ Html.text "play/pause" ]
        , Html.button [ onClick <| Perturb Bother ] [ Html.text "bother" ]
        , Html.button [ onClick <| Perturb Brake ] [ Html.text "brake" ]
        , Html.button [ onClick <| Perturb Faster ] [ Html.text "faster" ]
        , Html.table [ HtmlA.style [ ( "border", "1px solid black" ), ( "width", "100%" ), ( "table-layout", "fixed" ) ] ]
            [ Html.tr []
                [ Html.td [ tdStyleLeft ] [ Html.text "fps" ]
                , Html.td [ tdStyleRight ] [ Html.text <| Round.round 2 (1000 / dt) ]
                ]
            , Html.tr []
                [ Html.td [ tdStyleLeft ] [ Html.text "scale" ]
                , Html.td [ tdStyleRight ] [ Html.text <| Round.round 10 projection.scale ]
                ]
            , Html.tr []
                [ Html.td [ tdStyleLeft ] [ Html.text "eccentricity" ]
                , Html.td [ tdStyleRight ] [ Html.text "" ]
                ]
            , Html.tr []
                [ Html.td [ tdStyleLeft ] [ Html.text "kinetic energy" ]
                , Html.td [ tdStyleRight ]
                    [ kinetic
                        -- |> Round.round 6
                        |> toString
                        |> Html.text
                    ]
                ]
            , Html.tr []
                [ Html.td [ tdStyleLeft ] [ Html.text "potential energy" ]
                , Html.td [ tdStyleRight ]
                    [ potential
                        -- |> Round.round 6
                        |> toString
                        |> Html.text
                    ]
                ]
            , Html.tr []
                [ Html.td [ tdStyleLeft ] [ Html.text "total energy" ]
                , Html.td [ tdStyleRight ]
                    [ (kinetic + potential)
                        |> toString
                        -- |> Round.round 6
                        |> Html.text
                    ]
                ]
            ]
        ]


paneStyle =
    HtmlA.style
        [ ( "flex", "1" )
        , ( "padding", "1rem" )
        , ( "font-family", "Courier New" )
        , ( "font-size", "0.6em" )
        ]


tdStyleLeft =
    HtmlA.style [ ( "width", "80px" ), ( "word-wrap", "break-word" ) ]


tdStyleRight =
    HtmlA.style [ ( "width", "120px" ), ( "word-wrap", "break-word" ) ]



-- roundHelper : number -> Float -> String
-- roundHelper decimals num =
--     let
--         scale =
--             10 ^ decimals
--     in
--     (num * scale)
--         |> (round >> toFloat)
--         |> (\n -> n / scale)
--         |> FormatNumber.format { usLocale | decimals = decimals }


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


constG : Float
constG =
    6.67408e-11


applyN : Int -> (a -> a) -> a -> a
applyN n fun input =
    if n <= 0 then
        input
    else
        applyN (n - 1) fun (fun input)


euler : Float -> ( Body, Body ) -> Body
euler dt ( fixedBody, movingBody ) =
    let
        ( x, y, z ) =
            Point3d.coordinates movingBody.position

        ( u, v, w ) =
            Vector3d.components movingBody.velocity

        quot =
            dt * constG * fixedBody.mass / (x ^ 2 + y ^ 2 + z ^ 2) ^ (3 / 2)

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


kineticEnergy : { a | mass : Mass, velocity : Vector3d } -> Float
kineticEnergy thing =
    0.5 * thing.mass * Vector3d.squaredLength thing.velocity


potentialEnergy : Body -> Body -> Float
potentialEnergy body1 body2 =
    let
        r =
            Point3d.distanceFrom body1.position body2.position
    in
    -constG * body1.mass * body2.mass / (2 * r)



-- gravitationalPotential :
--     { a | mass : Mass, position : Point3d }
--     -> { b | position : Point3d }
--     -> Float
-- gravitationalPotential thing body =
--     let
--         r =
--             Point3d.distanceFrom thing.position body.position
--     in
--     thing.mass / r
--


drawing : Model -> Html Msg
drawing { earth, moon, trail, projection } =
    let
        sketchPlane =
            SketchPlane3d.xy

        everything =
            (Svg.rect [ x "-500", y "-300", width "100%", height "100%", SvgA.fill "#f7f7f7" ] [] :: [])
                ++ List.map (drawTrail projection) trail
                ++ [ drawBody projection earth, drawBody projection moon ]
    in
    Svg.svg
        [ width "1000", height "600", SvgA.viewBox "-500 -300 1000 600", onWheel Zoom ]
        everything



--


drawBody : Projection -> Body -> Svg msg
drawBody { plane, center, scale } { mass, position, radius, atmosphere } =
    let
        scaledRadius =
            radius * scale

        scaledPosition =
            position
                |> Point3d.projectInto plane
                |> Point2d.scaleAbout center scale

        atmocircle =
            if atmosphere /= 0 then
                [ Svg.circle2d [ SvgA.fill "#adc5ed", SvgA.opacity "0.35" ]
                    (Circle2d.withRadius (scaledRadius + 20) scaledPosition)
                ]
            else
                [ Svg.circle2d [ SvgA.fill "#fff200", SvgA.opacity "0.35" ]
                    (Circle2d.withRadius (scaledRadius + 10) scaledPosition)
                ]
    in
    Svg.g []
        (atmocircle
            ++ [ Svg.circle2d [ SvgA.fill "#666" ]
                    (Circle2d.withRadius scaledRadius scaledPosition)
               ]
        )


drawTrail : Projection -> Point3d -> Svg msg
drawTrail { plane, center, scale } position =
    let
        scaledPosition =
            position
                |> Point3d.projectInto plane
                |> Point2d.scaleAbout center scale
    in
    Svg.g []
        [ Svg.circle2d [ SvgA.fill "#fff" ]
            (Circle2d.withRadius 2 scaledPosition)
        ]
