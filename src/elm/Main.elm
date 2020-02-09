module Main exposing (main)

import Browser
import Canvas exposing (..)
import Canvas.Settings exposing (fill)
import Color
import Html exposing (Html, div, h2)
import Html.Events.Extra.Mouse exposing (onClick)
import ParticlesGenerator exposing (Dim2D, ParticlesGenerator, createParticlesGenerator, evolve, render)
import Random exposing (Generator)
import Task
import Time exposing (Posix, every, now, posixToMillis)


type alias Model =
    { dim : Dim2D
    , particleGenerators : List ParticlesGenerator
    , lastUpdate : Int
    , seed : Random.Seed
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
                | particleGenerators = createParticlesGenerator model.dim pos :: model.particleGenerators
              }
            , Cmd.none
            )

        InitTime time ->
            ( { model
                | lastUpdate = posixToMillis time
                , seed = Random.initialSeed <| posixToMillis time
              }
            , Cmd.none
            )

        Tick time ->
            let
                deltaMs =
                    posixToMillis time - model.lastUpdate
            in
            ( { model
                | lastUpdate = posixToMillis time
                , seed = Random.initialSeed <| posixToMillis time
                , particleGenerators = evolve deltaMs model.seed model.particleGenerators
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )



-- -------- View ----------- --


viewApp : Model -> Html Msg
viewApp model =
    let
        ( dimx, dimy ) =
            model.dim

        dim =
            ( floor dimx, floor <| dimy )
    in
    div []
        [ h2 [] [ Html.text "Elm Canvas Animate " ]
        , Canvas.toHtml dim
            [ onClick (.offsetPos >> AddParticlesGenerator) ]
            (shapes [ fill <| Color.rgb255 10 10 10 ] [ rect ( 0, 0 ) dimx dimy ]
                :: render model.particleGenerators
            )
        ]



-- -------- Init ----------- --


initApp : Dim -> ( Model, Cmd Msg )
initApp d =
    ( { dim = ( d.w, d.h )
      , particleGenerators = []
      , lastUpdate = 0
      , seed = Random.initialSeed 0
      }
    , Task.perform InitTime Time.now
    )


main : Program Dim Model Msg
main =
    Browser.element
        { init = initApp
        , view = viewApp
        , update = update
        , subscriptions = \_ -> every (1000 / 60) Tick
        }
