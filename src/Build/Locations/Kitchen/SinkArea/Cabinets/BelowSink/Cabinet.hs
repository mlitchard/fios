module Build.Locations.Kitchen.SinkArea.Cabinets.BelowSink.Cabinet
  where

import Game.Model.Mapping ( Label(..) )
import Game.Model.World
import Game.World
import qualified Data.Map.Strict
import Build.ObjectTemplate (kitchenCabinetBelowSinkGID)
import Build.Locations.Kitchen.SinkArea.Cabinets.BelowSink.Actions.Close
import Build.Locations.Kitchen.SinkArea.Cabinets.BelowSink.Actions.Look 
import Build.Locations.Kitchen.SinkArea.Cabinets.BelowSink.Actions.NoCanDo
import Build.Locations.Kitchen.SinkArea.Cabinets.BelowSink.Actions.Open
import Build.Locations.Kitchen.SinkArea.Cabinets.BelowSink.Actions.Put
import Tokenizer.Data ( Lexeme(CABINET) )
import Game.Object (setObjectMapM, getAnchoredTo)
import Build.LocationTemplate (kitchenGID)

buildKitchenCabinetBelowSink :: GameStateExceptT ()
buildKitchenCabinetBelowSink = do
  setObjectMapM kitchenCabinetBelowSinkGID buildCabinet
  initContainerMapM kitchenCabinetBelowSinkGID cabinetContainer

buildCabinet :: Object
buildCabinet = Object {
      _shortName' = "cabinet"
    , _entityLabel' = Label CABINET
    , _odescription' = [desc]
    , _descriptives' = []
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
orientation = AnchoredTo' anchoredTo

anchoredTo :: GameStateExceptT AnchoredTo
anchoredTo = 
  getAnchoredTo kitchenGID kitchenCabinetBelowSinkGID
  
cabinetContainer :: Container
cabinetContainer = Container Data.Map.Strict.empty