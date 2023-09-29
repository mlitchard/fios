module Build.Locations.Kitchen.SinkArea.Cabinets.AboveSink.Cabinet where

import Game.Model.Mapping 
        (GIDToDataMap (..), ContainerMap (..), Label (..))
import Game.Model.World
import qualified Data.Map.Strict
import Build.ObjectTemplate (kitchenCabinetAboveSinkGID, kitchenSinkGID)
import Game.Model.Condition 
        (Moveability(..), Perceptibility (..), Proximity (PlacedAbove))

import Tokenizer (Lexeme (CABINET))
import Game.World (initContainerMapM)
import Build.Locations.Kitchen.SinkArea.Cabinets.AboveSink.Actions.NoCanDo
import Build.Locations.Kitchen.Exits.EastExit.Door.Actions.Look 
        (initialLookAction)
import Build.Locations.Kitchen.SinkArea.Cabinets.AboveSink.Actions.Put
        ( putAction )
import Build.Locations.Kitchen.SinkArea.Cabinets.AboveSink.Actions.Open 
        (openAction)
import Build.Locations.Kitchen.SinkArea.Cabinets.BelowSink.Actions.Close 
        (closeAction)
import Game.Object (setObjectMapM)

buildKitchenCabinetAboveSink :: GameStateExceptT ()
buildKitchenCabinetAboveSink = do
  setObjectMapM kitchenCabinetAboveSinkGID buildCabinet
  initContainerMapM kitchenCabinetAboveSinkGID cabinetContainer

buildCabinet :: Object 
buildCabinet = Object { 
      _shortName' = "cabinet"
    , _entityLabel' = Label CABINET
    , _odescription' = [desc]
    , _descriptives' = []
    , _moveability' = NotMoveable
    , _orientation' = orientation
    , _standardActions' = standardActions
  }
  where 
    desc = "You can put things in it."

standardActions :: StandardActions
standardActions = StandardActions { 
    _getAction' = getAction
  , _putAction' = putAction 
  , _lookAction' = initialLookAction
  , _openAction' = openAction
  , _closeAction' = closeAction 
  , _lockAction' = lockAction
  , _unlockAction' = unlockAction
  , _goAction' = goAction
}

orientation :: Orientation 
orientation = AnchoredTo' (kitchenSinkGID, PlacedAbove)
cabinetContainer :: Container
cabinetContainer = Container 
  $ ContainerMap Data.Map.Strict.empty