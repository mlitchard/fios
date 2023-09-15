module HauntedHouse.Build.Locations.Kitchen.SinkArea.Cabinets.BelowSink where

import HauntedHouse.Game.Model.Mapping (GIDToDataMapping (..), ContainerMap (..))
import HauntedHouse.Game.Model.World
    
import qualified Data.Map.Strict
import HauntedHouse.Build.ObjectTemplate (kitchenCabinetBelowSinkGID, kitchenSinkGID)
import Data.These (These(..))
import HauntedHouse.Game.Model.Condition 
  (Moveability(..), Perceptibility (..), Proximity (PlacedUnder))
import HauntedHouse.Game.Actions.Close (standardCloseM)
import HauntedHouse.Game.Actions.Open (standardOpenM)
import HauntedHouse.Game.Actions.Get (noGetM)
import HauntedHouse.Game.Model.Mapping (Label (..))
import HauntedHouse.Tokenizer (Lexeme (CABINET))
import HauntedHouse.Game.Actions.Look (lookIn)

buildKitchenCabinetBelowSink :: GameStateExceptT ()
buildKitchenCabinetBelowSink = do
  world <- _world' <$> get 
  let objectMap' :: GIDToDataMapping Object 
      objectMap' = 
        GIDToDataMapping 
          $ Data.Map.Strict.insert 
              kitchenCabinetBelowSinkGID buildCabinet 
                $ (_unGIDToDataMapping' . _objectMap') world
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
    , _mNexus' = (Just . Containment') cabinetContainer
    , _standardActions' = standardActions
  }
  where
    desc = "You can put things in it."

standardActions :: StandardActions
standardActions = StandardActions 
  { _get' = const pass
  , _put' = const pass 
  , _lookIn' = lookIn
  }

orientation :: Orientation 
orientation = AnchoredTo' (kitchenSinkGID, PlacedUnder)

cabinetContainer :: Containment
cabinetContainer = (Containment . This) containedIn

containedIn :: ContainedIn
containedIn = ContainedIn { 
      _containerInterface' = containerInterface
    , _containedIn' = ContainerMap Data.Map.Strict.empty  
  }
  
containerInterface :: ContainerInterface
containerInterface = ContainerInterface {
      _openState'     = Closed 
    , _describe' = mempty
    , _openAction'    = standardOpenM kitchenCabinetBelowSinkGID
    , _closeAction'   = standardCloseM kitchenCabinetBelowSinkGID
    , _lockAction'    = pass 
    , _unlockAction'  = pass
  }   