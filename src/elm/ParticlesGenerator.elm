module ParticlesGenerator exposing
    ( Dim2D
    , ParticlesGenerator
    , Pos2D
    , createParticlesGenerator
    , evolve
    , render
    )

import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (shadow)
import Color exposing (Color)
import Random


type alias Dim2D =
    ( Float, Float )


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
    , dimension : Dim2D
    }


createFrquency =
    15


hitFriction =
    0.9


viscosity =
    0.9995


particleRad =
    12.0


gravity =
    9.8


lifetime =
    1500



-- ------------  GENERATE -------------- ---
-- generate : Int -> List ParticlesGenerator -> List Random.Generator ( Int, Int )
-- generate deltaMs =
--     List.Map Random.pair Random.float
-- -------------- EVOLVE --------------- ---


evolve : Int -> Random.Seed -> List ParticlesGenerator -> List ParticlesGenerator
evolve deltaMs seed =
    evolveGenerator deltaMs seed |> List.map


evolveGenerator : Int -> Random.Seed -> ParticlesGenerator -> ParticlesGenerator
evolveGenerator deltaMs seed pgen =
    let
        -- Decide if we need to generate a new particle
        particles =
            if remainderBy pgen.createFrequency pgen.tick == 0 then
                newParticle seed pgen :: pgen.particles

            else
                pgen.particles
    in
    { pgen
        | particles = evolveParticles pgen.dimension deltaMs particles
        , tick = pgen.tick + 1
    }


liveParticle : Particle -> Bool
liveParticle p =
    p.tickLeft >= 0


evolveParticles : Dim2D -> Int -> List Particle -> List Particle
evolveParticles dim deltaMs =
    List.filter liveParticle << List.map (evolveParticle dim deltaMs)


evolveParticle : Dim2D -> Int -> Particle -> Particle
evolveParticle ( size, floor ) deltaMs particle =
    let
        ( px, py ) =
            movePos deltaMs particle.position particle.velocity

        newPos =
            ( if px < 0 then
                -1 * px

              else if px > size then
                px - (px - size)

              else
                px
            , if py > floor then
                py - (py - floor)

              else
                py
            )

        ( vx, vy ) =
            newVel deltaMs particle.velocity

        hit =
            if px <= 0 || py >= floor || px >= size then
                hitFriction

            else
                1.0

        newVelocity =
            ( (if px <= 0 || px >= size then
                -1.0 * vx

               else
                vx
              )
                * hit
            , (if py > floor then
                -1 * vy

               else
                vy
              )
                * hit
            )
    in
    { position = newPos
    , velocity = friction <| newVelocity
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
    , Tuple.second vel + (gravity * deltaSec)
    )


friction : Pos2D -> Pos2D
friction ( vx, vy ) =
    ( vx * viscosity, vy * viscosity )


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

        --        , shadow { blur = 1.0, color = Color.black, offset = ( 1.0, 1.0 ) }
        ]
        (circle gen.position 4.0
            :: renderParticles gen.particles
        )


renderParticle : Particle -> Shape
renderParticle { position, tickLeft } =
    let
        rad =
            if tickLeft < 1000 then
                particleRad * (toFloat tickLeft / 1000.0)

            else
                particleRad
    in
    circle position rad


renderParticles : List Particle -> List Shape
renderParticles =
    List.map renderParticle


to255 : Int -> Int
to255 =
    remainderBy 200 >> (+) 50


pos2DToColor : Pos2D -> Color
pos2DToColor pos =
    Color.rgb255
        (Tuple.first pos |> round |> to255)
        (Tuple.second pos |> round |> to255)
        ((Tuple.first pos * Tuple.second pos) |> round |> to255)


createParticlesGenerator : Dim2D -> Pos2D -> ParticlesGenerator
createParticlesGenerator dim pos =
    { position = pos
    , createFrequency = createFrquency
    , tick = 0
    , particles = []
    , color = pos2DToColor pos
    , dimension = dim
    }


newParticle : Random.Seed -> ParticlesGenerator -> Particle
newParticle seed pg =
    let
        res =
            Random.step (Random.pair (Random.float -pi pi) (Random.float 8.0 200.0)) seed

        ( angle, intensity ) =
            Tuple.first res

        -- Random.pair (Random.float 0.2 0.6) (Random.float -0.1 -0.6)
    in
    { position = pg.position
    , velocity = ( intensity * cos angle, intensity * sin angle )
    , color = pg.color
    , tickLeft = lifetime
    }
