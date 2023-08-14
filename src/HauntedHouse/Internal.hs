module HauntedHouse.Internal where 
import HauntedHouse.Game.Model 
        (GameStateExceptT, Verbosity (..), GameState (..))
import Control.Monad.Except (throwError)

throwMaybeM :: Text -> Maybe a -> GameStateExceptT a
throwMaybeM _ (Just a) = pure a
throwMaybeM errmsg Nothing  = throwError errmsg

throwEitherM :: Text -> Either a b -> GameStateExceptT b 
throwEitherM _ (Right b)     = pure b
throwEitherM errmsg (Left _) = throwError errmsg 

setVerbosityM :: GameStateExceptT ()
setVerbosityM = do
  (verbosity,report) <- setVerbose . _verbosity' <$> get
  liftIO $ print report
  modify' (\gs -> gs{_verbosity' = verbosity})
  pass

setVerbose :: Verbosity -> (Verbosity,Text)
setVerbose Quiet = (Normal, "verbosity set to normal")
setVerbose Normal = (Loud, "verbosity set to loud") 
setVerbose Loud = (Quiet, "verbosity set to quiet")