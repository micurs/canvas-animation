module Main exposing (main)

import Browser
import Canvas exposing (..)
import Canvas.Settings exposing (fill)
import Color
import Html exposing (Html, div, h2)
import Html.Events.Extra.Mouse exposing (onClick)
import ParticlesGenerator exposing (ParticlesGenerator, createParticlesGenerator, evolve, render)
import Task
import Time exposing (Posix, every, now, posixToMillis)


type alias Model =
    { dim : Dim
    , generators : List ParticlesGenerator
    , lastUpdate : Int
    }


type alias Dim =
    { w : Float
    , h : Float
    }


type Msg
    = Init
    | InitTime Posix
    | AddParticlesGenerator ( Float, Float )
    | Tick Posix



-- -------- Update --------- --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddParticlesGenerator pos ->
            ( { model
                | generators = createParticlesGenerator pos :: model.generators
              }
            , Cmd.none
            )

        InitTime time ->
            ( { model | lastUpdate = posixToMillis time }, Cmd.none )

        Tick time ->
            let
                deltaMs =
                    posixToMillis time - model.lastUpdate
            in
            ( { model
                | lastUpdate = posixToMillis time
                , generators = evolve deltaMs model.generators
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )



-- -------- View ----------- --


viewApp : Model -> Html Msg
viewApp model =
    div []
        [ h2 [] [ Html.text "Elm Canvas Animate " ]
        , Canvas.toHtml ( floor model.dim.w, floor model.dim.h )
            [ onClick (.offsetPos >> AddParticlesGenerator) ]
            (shapes [ fill Color.darkBlue ] [ rect ( 0, 0 ) model.dim.w model.dim.h ]
                :: render model.generators
            )
        ]



-- -------- Init ----------- --


initApp : Dim -> ( Model, Cmd Msg )
initApp d =
    ( { dim = d
      , generators = []
      , lastUpdate = 0
      }
    , Task.perform InitTime now
    )


main : Program Dim Model Msg
main =
    Browser.element
        { init = initApp
        , view = viewApp
        , update = update
        , subscriptions = \_ -> every 10 Tick
        }
