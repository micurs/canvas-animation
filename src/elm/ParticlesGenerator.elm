module ParticlesGenerator exposing
    ( ParticlesGenerator
    , Pos2D
    , createParticlesGenerator
    , evolve
    , render
    )

import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (shadow)
import Color exposing (Color)


type alias Pos2D =
    ( Float, Float )


type alias Particle =
    { position : Pos2D
    , velocity : Pos2D
    , tickLeft : Int
    , color : Color
    }


type alias ParticlesGenerator =
    { position : Pos2D
    , createFrequency : Int
    , tick : Int
    , particles : List Particle
    , color : Color
    }



-- ------------  GENERATE -------------- ---
-- generate : Int -> List ParticlesGenerator -> List Random.Generator ( Int, Int )
-- generate deltaMs =
--     List.Map Random.pair Random.float
-- -------------- EVOLVE --------------- ---


evolve : Int -> List ParticlesGenerator -> List ParticlesGenerator
evolve deltaMs =
    evolveGenerator deltaMs |> List.map


evolveGenerator : Int -> ParticlesGenerator -> ParticlesGenerator
evolveGenerator deltaMs pgen =
    let
        particles =
            if remainderBy pgen.createFrequency pgen.tick == 0 then
                newParticle pgen :: pgen.particles

            else
                pgen.particles
    in
    { pgen
        | particles = evolveParticles deltaMs particles
        , tick = pgen.tick + 1
    }


liveParticle : Particle -> Bool
liveParticle p =
    p.tickLeft >= 0


evolveParticles : Int -> List Particle -> List Particle
evolveParticles deltaMs =
    List.filter liveParticle << List.map (evolveParticle deltaMs)



-- <<
--     List.map <|
--         evolveParticle deltaMs


evolveParticle : Int -> Particle -> Particle
evolveParticle deltaMs particle =
    { position = movePos deltaMs particle.position particle.velocity
    , velocity = newVel deltaMs <| friction particle.velocity
    , tickLeft = particle.tickLeft - 1
    , color = particle.color
    }


newVel : Int -> Pos2D -> Pos2D
newVel deltaMs vel =
    let
        deltaSec =
            toFloat deltaMs / 1000
    in
    ( Tuple.first vel
    , Tuple.second vel + (9.8 * deltaSec)
    )


friction : Pos2D -> Pos2D
friction p =
    ( Tuple.first p * 0.999, Tuple.second p * 0.9995 )



-- ( Tuple.first p * 0.995, Tuple.second p * 0.995 )


movePos : Int -> Pos2D -> Pos2D -> Pos2D
movePos deltaMs pos vel =
    let
        deltaSec =
            toFloat deltaMs / 1000
    in
    ( deltaSec * Tuple.first vel + Tuple.first pos
    , deltaSec * Tuple.second vel + Tuple.second pos
    )



-- -------------- RENDER --------------- ---


render : List ParticlesGenerator -> List Renderable
render gs =
    List.map renderGenerator gs


renderGenerator : ParticlesGenerator -> Renderable
renderGenerator gen =
    shapes
        [ fill gen.color

        -- , shadow { blur = 1.0, color = Color.black, offset = ( 1.0, 1.0 ) }
        ]
        (circle gen.position 3
            :: renderParticles gen.particles
        )


renderParticle : Particle -> Shape
renderParticle { position } =
    circle position 3.0


renderParticles : List Particle -> List Shape
renderParticles =
    List.map renderParticle


to255 : Int -> Int
to255 =
    remainderBy 255


pos2DToColor : Pos2D -> Color
pos2DToColor pos =
    Color.rgb255
        (Tuple.first pos |> round |> to255)
        (Tuple.second pos |> round |> to255)
        ((Tuple.first pos + Tuple.second pos) |> round |> to255)


createParticlesGenerator : Pos2D -> ParticlesGenerator
createParticlesGenerator pos =
    { position = pos
    , createFrequency = 50
    , tick = 0
    , particles = []
    , color = pos2DToColor pos
    }


newParticle : ParticlesGenerator -> Particle
newParticle pg =
    { position = pg.position
    , velocity = ( -1 * (Tuple.first pg.position - 250) / 5, Tuple.second pg.position * -0.3 )
    , color = pg.color
    , tickLeft = 5000
    }


generatorTick : Int -> ParticlesGenerator -> ParticlesGenerator
generatorTick tick pg =
    if remainderBy tick pg.createFrequency == 0 then
        { pg | particles = newParticle pg :: pg.particles }

    else
        pg
