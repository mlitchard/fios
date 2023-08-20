module HauntedHouse.Clarifier where

import Control.Monad.Except ( MonadError(throwError) )
import HauntedHouse.Game.Model.World (Object, GameStateExceptT, GameState (..))
import HauntedHouse.Game.Model.GID (GID)

clarifier :: Text -> GameStateExceptT ()
clarifier report = do 
  currentReport <- _report' <$> get
  modify' (\gs -> gs{_report' = currentReport <> [report]})

clarifyWhich :: NonEmpty (GID Object) -> GameStateExceptT () 
clarifyWhich _fuzzyObjects = pass 

clarifyNotThere :: GameStateExceptT () 
clarifyNotThere = throwError "You don't see that here"

