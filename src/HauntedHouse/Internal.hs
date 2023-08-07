module HauntedHouse.Internal where 
import HauntedHouse.Game.Model (GameStateExceptT)
import Control.Monad.Except (throwError)

throwMaybeM :: Text -> Maybe a -> GameStateExceptT a
throwMaybeM _ (Just a) = pure a
throwMaybeM errmsg Nothing  = throwError errmsg

throwEitherM :: Text -> Either a b -> GameStateExceptT b 
throwEitherM _ (Right b)     = pure b
throwEitherM errmsg (Left _) = throwError errmsg 