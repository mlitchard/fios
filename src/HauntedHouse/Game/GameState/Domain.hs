module HauntedHouse.Game.GameState.Domain where

import HauntedHouse.Game.Narration.Domain (Narration)
import HauntedHouse.Game.Player
import System.Console.Haskeline
import HauntedHouse.Game.World (World)

type GameStateT = StateT GameState IO
type GameStateExceptT = ExceptT Text GameStateT
type InputGameStateExceptT = InputT GameStateExceptT

data GameState = GameState
  { _world'         :: World
  , _report'        :: [Text]
  , _player'        :: PlayerData 
  , _narration'     :: Narration
  , _newScene'      :: Bool
  , _clarification' :: Maybe (NonEmpty Text)
  } deriving stock Show

