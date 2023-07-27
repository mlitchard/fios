module HauntedHouse.Game.Narration.Domain where

data Narration = Narration
  { _playerAction'         :: [Text]
  , _environmentResponse'  :: [Text]
  , _npAgentResponse'      :: [Text]
  , _scene'                :: [Text]
  }
  deriving stock (Show)
