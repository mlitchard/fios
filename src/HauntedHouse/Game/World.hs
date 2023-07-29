module HauntedHouse.Game.World where

import HauntedHouse.Game.Model
import Control.Monad.Except (throwError)

throwMaybe :: Text -> Maybe a -> GameStateExceptT a
throwMaybe _ (Just a) = pure a
throwMaybe errmsg Nothing  = throwError errmsg