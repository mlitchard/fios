module Build.Locations.Kitchen.SinkArea.Cabinets.BelowSink.Cabinet
  where

import Game.Model.Mapping
    ( GIDToDataMapping(..), ContainerMap(..), Label(..) )
import Game.Model.World
import Game.World
import qualified Data.Map.Strict
import Build.ObjectTemplate (kitchenCabinetBelowSinkGID, kitchenSinkGID)
import Build.Locations.Kitchen.SinkArea.Cabinets.BelowSink.Actions.Close
import Build.Locations.Kitchen.SinkArea.Cabinets.BelowSink.Actions.Look 
import Build.Locations.Kitchen.SinkArea.Cabinets.BelowSink.Actions.NoCanDo
import Build.Locations.Kitchen.SinkArea.Cabinets.BelowSink.Actions.Open
import Build.Locations.Kitchen.SinkArea.Cabinets.BelowSink.Actions.Put
import Game.Model.Condition
  (Moveability(..), Perceptibility (..), Proximity (PlacedUnder))
import Tokenizer (Lexeme (CABINET))

buildKitchenCabinetBelowSink :: GameStateExceptT ()
buildKitchenCabinetBelowSink = do
  world <- _world' <$> get
  let objectMap' :: GIDToDataMapping Object Object
      objectMap' =
        GIDToDataMapping
          $ Data.Map.Strict.insert
              kitchenCabinetBelowSinkGID buildCabinet
                $ (_unGIDToDataMapping' . _objectMap') world
  initContainerMapM kitchenCabinetBelowSinkGID cabinetContainer
  modify' (\gs -> gs{_world' = world{_objectMap' = objectMap'}})

buildCabinet :: Object
buildCabinet = Object {
      _shortName' = "cabinet"
    , _entityLabel' = Label CABINET
    , _odescription' = [desc]
    , _descriptives' = []
    , _moveability' = NotMoveable
    , _perceptability' = Perceptible
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
orientation = AnchoredTo' (kitchenSinkGID, PlacedUnder)

cabinetContainer :: Container
cabinetContainer = Container
  $ ContainerMap Data.Map.Strict.empty
