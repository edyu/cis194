{- Pong

   Definition and operations on Pong paddles
-}

module Pong.Paddle where

import Pong.Constants

import Graphics.Gloss

import Control.Arrow


-- | The paddle
data Paddle = Paddle { paddle_location :: Location
                     , paddle_color    :: Color
                     , paddle_width    :: Float
                     , paddle_height   :: Float
                     }

-- | Get the color of the paddle
paddleColor :: Paddle -> Color
paddleColor paddle = paddle_color paddle

-- | Get the width of the paddle
paddleWidth :: Paddle -> Float
paddleWidth paddle = paddle_width paddle

-- | Get the height of the paddle
paddleHeight :: Paddle -> Float
paddleHeight paddle = paddle_height paddle

-- | Move the paddle
movePaddle :: (Location -> Location) -> Paddle -> Paddle
movePaddle loc_f p@(Paddle { paddle_location = loc })
    = p { paddle_location = loc_f loc }

-- | Render the paddle at the given location.
renderPaddle :: Paddle -> Picture
renderPaddle paddle =
    color (paddleColor paddle) (polygon path)
  where
    (x, y) = paddle_location paddle
    fx = fromIntegral x
    fy = fromIntegral y
    width  = paddleWidth paddle
    height = (paddleHeight paddle) / 2
    path = [(fx, fy-height), (fx+width, fy-height), (fx+width, fy+height), (fx, fy+height)]

-- | Move a paddle down
paddleDown :: Paddle -> Paddle
paddleDown = movePaddle $ second (subtract moveStep)

-- | Move a paddle up
paddleUp :: Paddle -> Paddle
paddleUp = movePaddle $ second (+ moveStep)

validPaddle :: Paddle -> Bool
validPaddle p@(Paddle { paddle_location = loc })
    = let height = (paddleHeight p) / 2
          (x, y) = loc
      in  case fromIntegral y of
          fy | fy >= height && fy <= windowHeightF - height -> True
          _                                                 -> False
