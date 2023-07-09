module HauntedHouse.Game.Narration.Domain where

data Narration = Narration
  { playerAction :: Maybe (NonEmpty Text)
  , environmentResponse :: Maybe (NonEmpty Text)
  , npAgentResponse :: Maybe (NonEmpty Text)
  }
  deriving stock (Show)
