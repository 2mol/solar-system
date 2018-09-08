module EarthMoon exposing (main)

import Browser
import Browser.Events as Browser
import Circle2d
import Geometry.Svg as Svg
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (on, onClick)
import Json.Decode as Decode
import Plane3d
import Point2d exposing (Point2d)
import Round
import String
import Svg exposing (Svg)
import Svg.Attributes as SvgA
import TinyPlot as P exposing (TinyPlot)
import Vector2d exposing (Vector2d)


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { runState : RunState
    , earth : Body
    , moon : Body
    , trail : List Point2d
    , projection : Projection
    , fpsPlot : TinyPlot
    , kineticPlot : TinyPlot
    , potentialPlot : TinyPlot
    , totalEnergyPlot : TinyPlot
    }


type alias Body =
    { mass : Mass
    , position : Point2d
    , velocity : Vector2d
    , radius : Float
    , atmosphere : Float
    }


type alias Mass =
    Float


type alias Projection =
    { center : Point2d
    , scale : Float
    }


type RunState
    = Running
    | Paused


type Msg
    = Tick Float
    | Act Action
    | Zoom Int
    | KeyPress String


type Action
    = Brake
    | Faster
    | StartPause
    | Reset


init : ( Model, Cmd msg )
init =
    ( initModel, Cmd.none )


initModel : Model
initModel =
    { runState = Paused
    , earth = initEarth
    , moon = initMoon
    , trail = []
    , projection =
        { center = Point2d.fromCoordinates ( 0, 0 )
        , scale = 7.0e-7
        }
    , fpsPlot = P.new "fps" |> P.setYRange (P.Fixed ( 0, 120 ))
    , kineticPlot = P.new "kinetic energy"
    , potentialPlot = P.new "potential energy"
    , totalEnergyPlot = P.new "total energy" |> P.setMaxPoints 1000
    }


initEarth : Body
initEarth =
    { mass = 5.9722e24
    , position = Point2d.fromCoordinates ( 0, 0 )
    , velocity = Vector2d.fromComponents ( 0, 0 )
    , radius = 6371000
    , atmosphere = 0.05
    }


initMoon : Body
initMoon =
    { mass = 7.34767309e22
    , position = Point2d.fromCoordinates ( 384400000, 0 )
    , velocity = Vector2d.fromComponents ( 0, 1000 )
    , radius = 1737000
    , atmosphere = 0
    }


update : Msg -> Model -> ( Model, Cmd msg )
update msg ({ runState, earth, moon, trail, projection, fpsPlot, kineticPlot, potentialPlot, totalEnergyPlot } as model) =
    case msg of
        Tick dt ->
            let
                nSteps =
                    1500

                timeStep =
                    5000 / nSteps

                moon_ =
                    applyN nSteps (euler timeStep earth) moon

                fps =
                    1000 / dt

                kinetic =
                    kineticEnergy earth moon

                potential =
                    potentialEnergy moon earth
            in
            ( { model
                | moon = moon_
                , trail = List.take 250 <| moon.position :: trail
                , fpsPlot = P.pushData fpsPlot fps
                , kineticPlot = P.pushData kineticPlot kinetic
                , potentialPlot = P.pushData potentialPlot potential
                , totalEnergyPlot = P.pushData totalEnergyPlot (kinetic + potential)
              }
            , Cmd.none
            )

        KeyPress s ->
            case s of
                "Enter" ->
                    update (Act StartPause) model

                _ ->
                    ( model, Cmd.none )

        Act action ->
            handleAction action model

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


view : Model -> Browser.Document Msg
view model =
    let
        plots =
            [ P.draw ( 750, 80 ) "#0074D9" model.kineticPlot
            , P.draw ( 750, 80 ) "#FFDC00" model.potentialPlot
            , P.draw ( 750, 80 ) "#85144b" model.totalEnergyPlot
            , P.draw ( 750, 80 ) "pink" model.fpsPlot
            ]
                |> List.map (\e -> Html.div [] [ e ])

        buttons =
            [ Html.button [ onClick <| Act StartPause ] [ Html.text "play/pause" ]
            , Html.button [ onClick <| Act Reset ] [ Html.text "reset" ]
            , Html.text " - "
            , Html.button [ onClick <| Act Brake ] [ Html.text "brake" ]
            , Html.button [ onClick <| Act Faster ] [ Html.text "faster" ]
            ]
    in
    { title = "Solar"
    , body =
        [ Html.div
            [ style "display" "flex"
            , style "flex-direction" "column"
            , style "align-items" "center"
            ]
            [ drawing model
            , Html.div [ style "margin" "0.4em" ] buttons
            , Html.div [] plots
            ]
        ]
    }


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.map KeyPress


subscriptions : Model -> Sub Msg
subscriptions { runState } =
    let
        tick =
            if runState == Paused then
                Sub.none

            else
                Browser.onAnimationFrameDelta Tick
    in
    Sub.batch [ tick, Browser.onKeyPress keyDecoder ]



--


handleAction : Action -> Model -> ( Model, Cmd msg )
handleAction act ({ runState, earth, moon } as model) =
    case act of
        StartPause ->
            ( { model | runState = toggleRunState runState }, Cmd.none )

        Reset ->
            ( initModel, Cmd.none )

        Brake ->
            ( { model | moon = accelerate 0.8 moon }, Cmd.none )

        Faster ->
            ( { model | moon = accelerate 1.2 moon }, Cmd.none )


accelerate scale thing =
    { thing | velocity = Vector2d.scaleBy scale thing.velocity }


toggleRunState : RunState -> RunState
toggleRunState runState =
    case runState of
        Paused ->
            Running

        Running ->
            Paused


onWheel : (Int -> msg) -> Html.Attribute msg
onWheel message =
    on "wheel" (Decode.map message (Decode.at [ "deltaY" ] Decode.int))



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


euler : Float -> Body -> Body -> Body
euler dt fixedBody movingBody =
    let
        ( x, y ) =
            Point2d.coordinates movingBody.position

        ( u, v ) =
            Vector2d.components movingBody.velocity

        r =
            Point2d.squaredDistanceFrom
                fixedBody.position
                movingBody.position

        quot =
            dt * constG * fixedBody.mass / r ^ (3 / 2)

        newPosition =
            Point2d.fromCoordinates
                ( x + u * dt
                , y + v * dt
                )

        newVelocity =
            Vector2d.fromComponents
                ( u - x * quot
                , v - y * quot
                )
    in
    { movingBody
        | position = newPosition
        , velocity = newVelocity
    }


rungeKutta4 dt pos vel =
    0


kineticEnergy : Body -> Body -> Float
kineticEnergy fixBody movBody =
    let
        mu =
            1 / (1 / fixBody.mass + 1 / movBody.mass)
    in
    0.5 * movBody.mass * Vector2d.squaredLength movBody.velocity


potentialEnergy : Body -> Body -> Float
potentialEnergy body1 body2 =
    let
        r =
            Point2d.distanceFrom body1.position body2.position
    in
    -constG * body1.mass * body2.mass / r


drawing : Model -> Html Msg
drawing { earth, moon, trail, projection } =
    let
        everything =
            [ Svg.rect [ SvgA.x "-500", SvgA.y "-300", SvgA.width "100%", SvgA.height "100%", SvgA.fill "#f7f7f7" ] [] ]
                -- ++ List.map (drawTrail projection) trail
                ++ [ drawTrailFast projection trail ]
                ++ [ drawBody projection earth, drawBody projection moon ]
    in
    Svg.svg
        [ SvgA.width "1000", SvgA.height "600", SvgA.viewBox "-500 -300 1000 600" ]
        --, onWheel Zoom ]
        everything



--


drawBody : Projection -> Body -> Svg msg
drawBody { center, scale } { mass, position, radius, atmosphere } =
    let
        scaledRadius =
            radius * scale

        scaledPosition =
            position
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


drawTrail : Projection -> Point2d -> Svg msg
drawTrail { center, scale } position =
    let
        scaledPosition =
            position
                |> Point2d.scaleAbout center scale
    in
    Svg.g []
        [ Svg.circle2d [ SvgA.fill "#fff" ]
            (Circle2d.withRadius 2 scaledPosition)
        ]


drawTrailFast : Projection -> List Point2d -> Svg msg
drawTrailFast { center, scale } positions =
    let
        scaledPositions =
            positions
                |> List.map (Point2d.scaleAbout center scale)
                |> List.map Point2d.coordinates

        coordString =
            List.map (\( x, y ) -> String.fromFloat x ++ "," ++ String.fromFloat y) scaledPositions
                |> String.join " "
    in
    Svg.polyline [ SvgA.fill "none", SvgA.stroke "white", SvgA.strokeWidth "2", SvgA.points coordString ] []
