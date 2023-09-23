module Build.Locations.Kitchen.SinkArea.Cabinets.BelowSink.Actions.Open
  where 

import Game.Model.World
import Control.Monad.Except ( MonadError(throwError) )

openAction :: OpenAction 
openAction = OpenAction {
    _updateOpen' = throwError "openAction not finished"
  , _open' = const open
}

open :: GameStateExceptT ()
open = throwError ("open not completed")