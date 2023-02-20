port module Main exposing (..)

import Browser
import Browser.Dom
import Browser.Events
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes
import Random exposing (Generator, constant, float, list)
import Task


black =
    rgb255 0 0 0


blue =
    rgb255 65 105 255


green =
    rgb255 65 224 105


type alias Size =
    ( Float, Float )


type alias Position =
    ( Float, Float )


port scrollReceiver : (Position -> msg) -> Sub msg


type Msg
    = GotWindowSize Size
    | GotShapeList (List Shape)
    | GotScroll Position


type alias Shape =
    { x : Float
    , y : Float
    , z : Float
    , r : Float
    }


randomShape : Size -> Generator Shape
randomShape ( w, h ) =
    let
        radius =
            16.0

        padding =
            radius * 4

        h2 =
            h * 2.0
    in
    Random.map4 Shape
        (float radius (w - radius - padding))
        (float radius (h2 - radius - padding))
        (float 1 3)
        (constant radius)


randomShapeList : Int -> Size -> Generator (List Shape)
randomShapeList n windowSize =
    randomShape windowSize
        |> list n


type alias Model =
    { size : Maybe Size
    , pos : Position
    , shapes : List Shape
    }


initialModel : Model
initialModel =
    { size = Nothing
    , pos = ( 0, 0 )
    , shapes = []
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        getWindowSize : Browser.Dom.Viewport -> Size
        getWindowSize { viewport } =
            ( viewport.width, viewport.height )

        getWindowSizeCmd =
            Browser.Dom.getViewport |> Task.map getWindowSize |> Task.perform GotWindowSize
    in
    ( initialModel, getWindowSizeCmd )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize (\x y -> GotWindowSize ( toFloat x, toFloat y ))
        , scrollReceiver GotScroll
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotWindowSize windowSize ->
            let
                generateShapeListCmd =
                    Random.generate GotShapeList <| randomShapeList 100 windowSize
            in
            ( { model | size = Just windowSize }, generateShapeListCmd )

        GotShapeList shapes ->
            ( { model | shapes = shapes }, Cmd.none )

        GotScroll pos ->
            ( { model | pos = pos }, Cmd.none )


viewSection : Float -> Element msg -> Element msg
viewSection height_ inner =
    Element.el
        [ Background.color green
        , alpha 0.5
        , width <| fill
        , height <| px <| round height_
        ]
        inner


viewLoading : Element msg
viewLoading =
    Element.none


viewShape : Position -> Shape -> Element msg
viewShape ( _, scrollY ) { x, y, z, r } =
    let
        y2 =
            y + scrollY * -z * 0.2
    in
    Element.el
        [ Background.color blue
        , Border.rounded <| round r
        , htmlAttribute <| Html.Attributes.style "position" "absolute"
        , htmlAttribute <| Html.Attributes.style "left" (String.fromFloat x ++ "px")
        , htmlAttribute <| Html.Attributes.style "top" (String.fromFloat y2 ++ "px")
        , htmlAttribute <| Html.Attributes.style "filter" ("blur(" ++ String.fromFloat (3 - z) ++ "px)")
        , scale <| z
        , alpha <| (1 / 5) * z
        , width <| px <| round (r * 2)
        , height <| px <| round (r * 2)
        ]
        Element.none


viewShapes : Position -> List Shape -> Element msg
viewShapes pos shapes =
    shapes
        |> List.map (viewShape pos)
        |> Element.row []


viewMain : Position -> List Shape -> Size -> Element Msg
viewMain pos shapes ( _, height_ ) =
    Element.column
        [ width fill
        , behindContent <| viewShapes pos shapes
        ]
        [ viewSection height_ <|
            Element.el [ centerX, centerY, Font.size 48, Font.color black ] <|
                text "Scroll down"
        , viewSection height_ <|
            Element.el [ centerX, centerY, Font.size 48, Font.color black ] <|
                text "Did it work?"
        ]


view : Model -> Html Msg
view model =
    model.size
        |> Maybe.map (viewMain model.pos model.shapes)
        |> Maybe.withDefault viewLoading
        |> Element.layout []


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
