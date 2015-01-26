{- Pong

   Definition and operations on Pong ball
-}

module Pong.Ball where

import Pong.Constants
import Pong.Paddle

import Graphics.Gloss
import System.Random


-- | A velocity for the ball is composed of an x-component and a y-component
type Velocity = (Float, Float)

-- | The ball
data Ball = Ball { ball_location :: Location
                 , ball_color    :: Color
                 , ball_radius   :: Float
                 , ball_velocity :: Velocity
                 }

-- | Get the color of the ball
ballColor :: Ball -> Color
ballColor ball = ball_color ball

-- | Get the radius of the ball
ballRadius :: Ball -> Float
ballRadius ball = ball_radius ball

ballOut :: Ball -> Bool
ballOut b@(Ball { ball_location = (x, _) })
    = let r = round $ ballRadius b
      in  x < -r  || x > windowWidth + r

newBall :: RandomGen g => g -> (Ball, g)
newBall gen
    = let (deg, gen') = randomR (0.0, 45.0) gen
          dir = (round deg) `mod` 4
          toRad = pi / 180.0
          rad = case dir of
                0 -> deg * toRad
                1 -> (180 - deg) * toRad
                2 -> (180 + deg) * toRad
                3 -> (360 - deg) * toRad
          size = fromIntegral baseSize
          vx = cos rad
          vy = sin rad
      in  ((Ball { ball_location = origin
                 , ball_color    = red
                 , ball_radius   = size / 2
                 , ball_velocity = (vx, vy)
                 }),
           gen')

-- | Move the ball
moveBall :: Ball -> Ball
moveBall b@(Ball { ball_location = loc
                 , ball_velocity = vec
                 })
    | ballOut b = b  -- no need to do anything more if ball is already out
    | otherwise =
        let (x, y)   = loc
            (vx, vy) = vec
            d  = fromIntegral moveStep
            dx = round $ vx * d
            r  = round $ ballRadius b
        in  bounceBall $ b { ball_location = (x + dx, y) }

reboundBall :: Ball -> Ball
reboundBall b@(Ball { ball_velocity = vec })
    = let (vx, vy) = vec
      in  b { ball_velocity = (-vx, vy) }

bounceBall :: Ball -> Ball
bounceBall b@(Ball { ball_location = loc
                   , ball_velocity = vec
                   })
    = let (x, y)   = loc
          (vx, vy) = vec
          d  = fromIntegral moveStep
          dy = round $ vy * d
          r  = round $ ballRadius b
          upperY = windowHeight - r
          lowerY = r
      in  case y + dy of
              y' | y' > upperY ->
                  b { ball_location = (x, upperY)
                    , ball_velocity = (vx, -vy)
                    }
              y' | y' < lowerY ->
                  b { ball_location = (x, lowerY)
                    , ball_velocity = (vx, -vy)
                    }
              _ ->
                  b { ball_location = (x, y + dy) }

-- | Render the ball at the given location.
renderBall :: Ball -> Picture
renderBall ball =
    let (x, y) = ball_location ball
        fx     = fromIntegral x
        fy     = fromIntegral y
    in  translate fx fy $
        color (ballColor ball) (thickCircle thickness radius)
  where
    radius = ballRadius ball
    thickness = radius / 2
