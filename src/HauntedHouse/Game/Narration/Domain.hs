module HauntedHouse.Game.Narration.Domain where

data Narration = Narration
  { _playerAction :: Maybe (NonEmpty Text)
  , _environmentResponse :: Maybe (NonEmpty Text)
  , _npAgentResponse :: Maybe (NonEmpty Text)
  , _scene :: Text
  }
  deriving stock (Show)
