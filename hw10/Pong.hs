{- Pong

   Main Pong game.
-}

module Pong where

import Graphics.Gloss.Interface.Pure.Game

import Data.Monoid
import System.Random

import Pong.Ball
import Pong.Constants
import Pong.Paddle


data State = NotStarted | Playing| Ended

data World = World { w_state  :: State
                   , w_ball   :: Ball
                   , w_paddle :: Paddle
                   , w_guard  :: Paddle
                   , w_up     :: Bool
                   , w_down   :: Bool
                   , w_gen    :: StdGen
                   }

initialWorld :: IO World
initialWorld = do
    gen <- newStdGen
    let size = fromIntegral baseSize
        (ball, gen') = newBall gen
        paddle = Paddle { paddle_location = rightMiddle
                        , paddle_color    = blue
                        , paddle_width    = size
                        , paddle_height   = size * 4
                        }
        guard  = Paddle { paddle_location = leftMiddle
                        , paddle_color    = blue
                        , paddle_width    = size
                        , paddle_height   = size * 4
                        }
    return $ World { w_state  = NotStarted
                   , w_ball   = ball
                   , w_paddle = paddle
                   , w_guard  = guard
                   , w_up     = False
                   , w_down   = False
                   , w_gen    = gen'
                   }

step :: Float -> World -> World
step elapsed w@(World { w_state  = Playing
                      , w_ball   = ball
                      , w_paddle = paddle
                      , w_guard  = guard
                      , w_up     = up
                      , w_down   = down
                      })
    | up && validPaddle paddle_moved_up
    = w { w_ball   = ball_moved
        , w_paddle = paddle_moved_up
        , w_guard  = guard_moved
        }
    | down && validPaddle paddle_moved_down
    = w { w_ball   = ball_moved
        , w_paddle = paddle_moved_down
        , w_guard  = guard_moved
        }
    | otherwise
    = w { w_state  = if (gameEnded ball) then Ended else Playing
        , w_ball   = ball_moved
        , w_paddle = paddle
        , w_guard  = guard_moved
        }
  where
    paddle_moved_up   = paddleUp paddle
    paddle_moved_down = paddleDown paddle
    guard_moved       = moveGuard guard ball
    ball_moved
        | detectHit guard paddle ball = moveBall $ reboundBall ball
        | otherwise                   = moveBall ball

step _ w = w  -- when not playing, don't step

gameEnded :: Ball -> Bool
gameEnded = ballOut

-- | React to a user event
react :: Event -> World -> World
react ev w@(World { w_state  = Playing
                  , w_paddle = paddle
                  , w_up     = up
                  , w_down   = down
                  })
    -- handle keypresses
    = case ev of
          EventKey (SpecialKey KeyUp) Down _ _ ->
              w { w_paddle = tryMovePaddle paddleUp paddle
                , w_up   = True
                , w_down = False
                }
          EventKey (SpecialKey KeyDown) Down _ _ ->
              w { w_paddle = tryMovePaddle paddleDown paddle
                , w_up   = False
                , w_down = True
                }
          EventKey (SpecialKey KeyUp) Up _ _ ->
              w { w_up = False }
          EventKey (SpecialKey KeyDown) Up _ _ ->
              w { w_down = False }
          EventKey (MouseButton WheelUp) Down _ _ ->
              w { w_paddle = tryMovePaddle paddleUp paddle }
          EventKey (MouseButton WheelDown) Down _ _ ->
              w { w_paddle = tryMovePaddle paddleDown paddle }
          _ -> w

-- handle spacebar when game is stopped
react (EventKey (SpecialKey KeySpace) Down _ _)
      w@(World { w_gen = gen })
    = let size = fromIntegral baseSize
          (ball, gen') = newBall gen
          paddle = Paddle { paddle_location = rightMiddle
                          , paddle_color    = blue
                          , paddle_width    = size
                          , paddle_height   = size * 4
                          }
          guard  = Paddle { paddle_location = leftMiddle
                          , paddle_color    = blue
                          , paddle_width    = size
                          , paddle_height   = size * 4
                          }
      in  w { w_state = Playing
          , w_ball  = ball
          , w_paddle = paddle
          , w_guard  = guard
          , w_up     = False
          , w_down   = False
          , w_gen    = gen'
          }

-- otherwise, ignore:
react _ w = w

detectHit :: Paddle -> Paddle -> Ball -> Bool
detectHit guard paddle ball
    = guardHit guard ball || paddleHit paddle ball

guardHit :: Paddle -> Ball -> Bool
guardHit (Paddle { paddle_location = ploc
                 , paddle_width    = pw
                 , paddle_height   = ph
                 })
         (Ball   { ball_location   = bloc
                 , ball_radius     = br
                 })
    = let (px, py) = ploc
          (bx, by) = bloc
          bsize    = round br
          psizeX   = (round pw) `div` 2
          psizeY   = (round ph) `div` 2
      in  bx - bsize <= px + psizeX &&
          by > py - psizeY && by < py + psizeY

paddleHit :: Paddle -> Ball -> Bool
paddleHit (Paddle { paddle_location = ploc
                  , paddle_width    = pw
                  , paddle_height   = ph
                  })
          (Ball   { ball_location   = bloc
                  , ball_radius     = br
                  })
    = let (px, py) = ploc
          (bx, by) = bloc
          bsize    = round br
          psizeX   = (round pw) `div` 2
          psizeY   = (round ph) `div` 2
      in  bx + bsize > px - psizeX &&
          by > py - psizeY && by < py + psizeY

moveGuard :: Paddle -> Ball -> Paddle
moveGuard g@(Paddle { paddle_location = (_, py) })
          b@(Ball { ball_location = (_, y)
                  , ball_velocity = (vx, _)
                  })
    | vx < 0    = if (py < y) then tryMovePaddle paddleUp g
                              else tryMovePaddle paddleDown g
    | otherwise = g
    


-- | Given a paddle transformer, try to move the paddle. If the move is
-- impossible, do nothing.
tryMovePaddle :: (Paddle -> Paddle) -> Paddle -> Paddle
tryMovePaddle mover paddle
    | validPaddle moved_paddle
    = moved_paddle

    | otherwise
    = paddle

  where
    moved_paddle = mover paddle

-- | Render the world into a picture.
render :: World -> Picture
render (World { w_state  = state
              , w_ball   = ball
              , w_paddle = paddle
              , w_guard  = guard
              })
      -- by default, (0, 0) is the center; I want it at the bottom left
    = translate (-windowWidthF / 2) (-windowHeightF / 2) $

      -- draw the ball
      (renderBall ball) <>

      -- draw the left paddle
      (renderPaddle guard) <>

      -- draw the right paddle
      (renderPaddle paddle) <>

      -- draw any instruction text
      (color black $
      scale 0.2 0.2 $
      renderState state)

-- | Render instruction text, based on the current state
renderState :: State -> Picture
renderState NotStarted = (translate 10 160 $ text "Spacebar to start") <>
                         (translate 10 20 $ text "ESC to quit")
renderState Playing    = blank
renderState Ended      = (translate 10 120 $ text "Spacebar to play again") <>
                         (translate 10 20 $ text "ESC to quit")

playPong:: IO ()
playPong = do
    world <- initialWorld
    play (InWindow "Tetris" (windowWidth, windowHeight) (windowX, windowY))
        background beatFrequency world render react step
