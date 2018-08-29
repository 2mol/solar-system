module Sup exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Circle2d
import Geometry.Svg as Svg
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (on, onClick)
import Json.Decode as Json
import Plane3d
import Plotting as P
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Round
import SketchPlane3d exposing (SketchPlane3d)
import String
import Svg exposing (Svg)
import Svg.Attributes as SvgA
import Vector3d exposing (Vector3d)


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { earth : Body
    , moon : Body
    , trail : List Point3d
    , runState : RunState
    , projection : Projection
    , frameTick : Float
    , fpsPlot : P.Plot Float
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
    , frameTick = 0
    , fpsPlot = P.emptyPlot 500 "fps"
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
update msg ({ runState, earth, moon, trail, projection, fpsPlot } as model) =
    case msg of
        Nope ->
            ( model, Cmd.none )

        Tick dt ->
            let
                nSteps =
                    500

                timeStep =
                    5000 / nSteps

                moon_ =
                    applyN nSteps (\m -> euler timeStep ( earth, m )) moon

                trail_ =
                    List.take 100 <| moon.position :: trail
            in
            ( { model
                | moon = moon_
                , trail = trail_
                , frameTick = dt
                , fpsPlot = P.addDataPoint fpsPlot (1000 / dt)
              }
            , Cmd.none
            )

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
    Html.div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        ]
        [ Html.div []
            [ Html.button [ onClick ToggleRunState ] [ Html.text "play/pause" ]
            , Html.button [ onClick <| Perturb Brake ] [ Html.text "brake" ]
            , Html.button [ onClick <| Perturb Faster ] [ Html.text "faster" ]
            ]
        , drawing model
        , P.draw ( 1000, 120 ) model.fpsPlot
        , physicsPane model
        ]


subscriptions : Model -> Sub Msg
subscriptions { runState } =
    if runState == Paused then
        Sub.none

    else
        onAnimationFrameDelta Tick



--


type Perturbation
    = Brake
    | Faster


annoy perturbation thing =
    case perturbation of
        Brake ->
            { thing | velocity = Vector3d.scaleBy 0.8 thing.velocity }

        Faster ->
            { thing | velocity = Vector3d.scaleBy 1.2 thing.velocity }



-- subviews


physicsPane : Model -> Html Msg
physicsPane { earth, moon, frameTick, projection } =
    let
        kinetic =
            kineticEnergy earth moon

        potential =
            potentialEnergy moon earth
    in
    Html.table
        [ style "padding" "0.5rem"
        , style "font-family" "Courier New"
        , style "font-size" "0.6em"
        , style "border" "1px solid black"
        , style "table-layout" "fixed"
        ]
        [ Html.tr []
            [ Html.td tdStyleLeft [ Html.text "fps" ]
            , Html.td tdStyleRight [ Html.text <| Round.round 2 (1000 / frameTick) ]
            ]
        , Html.tr []
            [ Html.td tdStyleLeft [ Html.text "scale" ]
            , Html.td tdStyleRight [ Html.text <| Round.round 10 projection.scale ]
            ]
        , Html.tr []
            [ Html.td tdStyleLeft [ Html.text "kinetic energy" ]
            , Html.td tdStyleRight
                [ kinetic
                    |> String.fromFloat
                    |> Html.text
                ]
            ]
        , Html.tr []
            [ Html.td tdStyleLeft [ Html.text "potential energy" ]
            , Html.td tdStyleRight
                [ potential
                    |> String.fromFloat
                    |> Html.text
                ]
            ]
        , Html.tr []
            [ Html.td tdStyleLeft [ Html.text "total energy" ]
            , Html.td tdStyleRight
                [ (kinetic + potential)
                    |> String.fromFloat
                    |> Html.text
                ]
            ]
        ]


tdStyleLeft =
    []


tdStyleRight =
    [ style "width" "160px" ]


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

        newPos =
            Point3d.fromCoordinates
                ( x + u * dt
                , y + v * dt
                , z + w * dt
                )

        newVel =
            Vector3d.fromComponents
                ( u - x * quot
                , v - y * quot
                , w - z * quot
                )
    in
    { movingBody
        | position = newPos
        , velocity = newVel
    }


kineticEnergy : Body -> Body -> Float
kineticEnergy fixBody movBody =
    let
        mu =
            1 / (1 / fixBody.mass + 1 / movBody.mass)
    in
    0.5 * mu * Vector3d.squaredLength movBody.velocity


potentialEnergy : Body -> Body -> Float
potentialEnergy body1 body2 =
    let
        r =
            Point3d.distanceFrom body1.position body2.position
    in
    -constG * body1.mass * body2.mass / r


drawing : Model -> Html Msg
drawing { earth, moon, trail, projection } =
    let
        sketchPlane =
            SketchPlane3d.xy

        everything =
            [ Svg.rect [ SvgA.x "-500", SvgA.y "-300", SvgA.width "100%", SvgA.height "100%", SvgA.fill "#f7f7f7" ] [] ]
                -- ++ List.map (drawTrail projection) trail
                ++ [drawTrailFast projection trail]
                ++ [ drawBody projection earth, drawBody projection moon ]
    in
    Svg.svg
        [ SvgA.width "1000", SvgA.height "600", SvgA.viewBox "-500 -300 1000 600" ]
        --, onWheel Zoom ]
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

drawTrailFast : Projection -> List Point3d -> Svg msg
drawTrailFast { plane, center, scale } positions =
    let
        scaledPositions =
            positions
                |> List.map (Point3d.projectInto plane)
                |> List.map (Point2d.scaleAbout center scale)
                |> List.map Point2d.coordinates

        coordString =
            List.map (\(x, y) -> String.fromFloat x ++ "," ++ String.fromFloat y) scaledPositions
                |> String.join " "
    in
    Svg.polyline [ SvgA.fill "none", SvgA.stroke "white", SvgA.strokeWidth "2", SvgA.points coordString ] []