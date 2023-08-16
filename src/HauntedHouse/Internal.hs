module HauntedHouse.Internal where 
import HauntedHouse.Game.Model 
        (GameStateExceptT, Verbosity (..), GameState (..))
import Control.Monad.Except (throwError)

throwMaybeM :: Text -> Maybe a -> GameStateExceptT a
throwMaybeM _ (Just a) = pure a
throwMaybeM errmsg Nothing  = throwError errmsg

throwLeftM :: Text -> Either (a :: Type) (b :: Type) -> GameStateExceptT b 
throwLeftM _ (Right b)     = pure b
throwLeftM errmsg (Left _) = throwError errmsg 

throwRightM :: Text -> Either (a :: Type) (b :: Type) -> GameStateExceptT a
throwRightM _ (Left a)       = pure a 
throwRightM errmsg (Right _) = throwError errmsg 

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