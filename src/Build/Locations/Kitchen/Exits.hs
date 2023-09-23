module Build.Locations.Kitchen.Exits where

import Build.ExitTemplate (kitchenEastExitGID)
import Game.World (setWorldExitMapM, setLocationDirectionM)
import Build.ObjectTemplate (kitchenEastDoorGID, kitchenEastPortalGID)
import Game.Object
    ( setObjectMapM, setObjectLabelMapM )
import Game.Model.Condition 
        (Moveability(..), Perceptibility (..), Proximity (PlacedBehind))
import Build.ObjectLabels
import Build.DirectionTemplate (eastLabel)
import Game.Actions.Get (noGetM)
import Control.Monad.Except (MonadError(..))
import Game.Model.Display (updateEnvironmentM)
import Tokenizer (Lexeme(DOOR))
import Game.Model.Mapping (Label(..))
import Game.Model.World
import Build.LocationTemplate (hallGID)
import Build.Locations.Kitchen.Exits.EastExit.DoorActions.Open
import Build.Locations.Kitchen.Exits.EastExit.PortalActions.Look

buildExits :: GameStateExceptT ()
buildExits =
  buildEastExit

buildEastExit :: GameStateExceptT ()
buildEastExit = do
  kitchenEastPortal
  kitchenEastDoor
 
