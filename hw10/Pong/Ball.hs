{- Pong

   Definition and operations on Pong ball
-}

module Pong.Ball where

import Pong.Constants

import Graphics.Gloss


-- | The ball
data Ball = Ball { ball_loc    :: Location
                 , ball_color  :: Color
                 , ball_radius :: Float
                 }

-- | Get the location of the ball
ballLoc :: Ball -> Location
ballLoc ball = ball_loc ball

-- | Get the color of the ball
ballColor :: Ball -> Color
ballColor ball = ball_color ball

-- | Get the radius of the ball
ballRadius :: Ball -> Float
ballRadius ball = ball_radius ball

-- | Move the ball
moveBall :: (Location -> Location) -> Ball -> Ball
moveBall loc_f b@(Ball { ball_loc = loc }) = b { ball_loc = loc_f loc }

-- | Render the ball at the given location.
renderBall :: Ball -> Picture
renderBall ball =
    color (ballColor ball) (thickCircle thickness radius)
  where
    radius = ballRadius ball
    thickness = radius / 2
