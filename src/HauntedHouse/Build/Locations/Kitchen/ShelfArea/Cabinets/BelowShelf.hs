{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use one" #-}
module HauntedHouse.Build.Locations.Kitchen.ShelfArea.Cabinets.BelowShelf 
  where
  
import HauntedHouse.Game.Model.Mapping (GIDToDataMapping (..), ContainerMap (..), GIDList, Label (..))
import HauntedHouse.Game.Model.World
import qualified Data.Map.Strict (insert, empty, singleton)
import HauntedHouse.Build.ObjectTemplate (kitchenCabinetBelowShelfGID, kitchenShelfGID, plantPotGID)
import Data.These (These(..))
import HauntedHouse.Game.Model.Condition 
        (Moveability(..), Perceptibility (..), Proximity (PlacedUnder))
import qualified Data.List.NonEmpty
import HauntedHouse.Tokenizer (Lexeme(PLANT, POT))
import HauntedHouse.Game.Actions.Open (standardOpenM)
import HauntedHouse.Game.Actions.Close (standardCloseM)
import HauntedHouse.Game.Actions.Get (noGetM)

buildKitchenCabinetBelowShelf :: GameStateExceptT ()
buildKitchenCabinetBelowShelf = do
  world <- _world' <$> get 
  let objectMap' :: GIDToDataMapping Object 
      objectMap' = 
        GIDToDataMapping 
          $ Data.Map.Strict.insert 
              kitchenCabinetBelowShelfGID buildCabinet 
                $ (_unGIDToDataMapping' . _objectMap') world
  modify' (\gs -> gs{_world' = world{_objectMap' = objectMap'}})

buildCabinet :: Object 
buildCabinet = Object { 
      _shortName' = "cabinet"
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
  { _get' = noGetM
  , _put' = pass 
  }

orientation :: Orientation 
orientation = AnchoredTo' (kitchenShelfGID,PlacedUnder)

cabinetContainer :: Containment
cabinetContainer = (Containment . This) containedIn

containedIn :: ContainedIn
containedIn = ContainedIn 
  {_containerInterface' = containerInterface
  , _containedIn' = ContainerMap 
                      $ Data.Map.Strict.singleton (Label POT) inCabinet 
  }

inCabinet :: GIDList Object
inCabinet = Data.List.NonEmpty.singleton plantPotGID

containerInterface :: ContainerInterface 
containerInterface = ContainerInterface {
      _openState'     = Closed 
    , _describe' = mempty
    , _openAction'    = standardOpenM kitchenCabinetBelowShelfGID 
    , _closeAction'   = standardCloseM kitchenCabinetBelowShelfGID
    , _lockAction'    = pass
    , _unlockAction'  = pass
  }
  