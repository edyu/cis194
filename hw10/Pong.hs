{- Pong

   Main Pong game.
-}

module Pong where

import Graphics.Gloss.Interface.Pure.Game

import Data.Monoid

import Pong.Constants


data State = NotStarted | Playing| Ended

data World = World { w_state :: State
                   }

initialWorld :: IO World
initialWorld = do
    return $ World { w_state = NotStarted
                   }

step :: Float -> World -> World
step elapsed w@(World { w_state = Playing
                      })
    = w { w_state = if gameEnded then Ended else Playing
        }

step _ w = w  -- when not playing, don't step

gameEnded :: Bool
gameEnded = False

-- | React to a user event
react :: Event -> World -> World
react ev w@(World { w_state = Playing
                  })
  -- handle keypresses
  = case ev of
        EventKey (SpecialKey KeyUp) Down _ _ ->
            w
        EventKey (SpecialKey KeyDown) Down _ _ ->
            w
        _ -> w

-- handle spacebar when game is stopped
react (EventKey (SpecialKey KeySpace) Down _ _)
      w
    = w { w_state = Playing
        }

-- otherwise, ignore:
react _ w = w

-- | Render the world into a picture.
render :: World -> Picture
render (World { w_state = state
              })
      -- by default, (0, 0) is the center; I want it at the bottom left
    = translate (-windowWidthF / 2) (-windowHeightF / 2) $

    -- draw any instruction text
    (color magenta $
     scale 0.2 0.2 $
     renderState state)

-- | Render instruction text, based on the current state
renderState :: State -> Picture
renderState NotStarted = (translate 0 120 $ text "Spacebar to") <>
                         text "start"
renderState Playing    = blank
renderState Ended      = (translate 0 120 $ text "Spacebar to") <>
                         text "play again"

playPong:: IO ()
playPong = do
    world <- initialWorld
    play (InWindow "Tetris" (windowWidth, windowHeight) (windowX, windowY))
        background beatFrequency world render react step