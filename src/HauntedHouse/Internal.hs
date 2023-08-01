module HauntedHouse.Internal where 
import HauntedHouse.Game.Model (GameStateExceptT)
import Control.Monad.Except (throwError)

throwMaybeM :: Text -> Maybe a -> GameStateExceptT a
throwMaybeM _ (Just a) = pure a
throwMaybeM errmsg Nothing  = throwError errmsg
