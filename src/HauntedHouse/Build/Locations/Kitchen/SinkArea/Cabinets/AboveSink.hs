module HauntedHouse.Build.Locations.Kitchen.SinkArea.Cabinets.AboveSink where

import HauntedHouse.Game.Model.Mapping 
        (GIDToDataMapping (..), ContainerMap (..), Label (..))
import HauntedHouse.Game.Model.World
import qualified Data.Map.Strict
import HauntedHouse.Build.ObjectTemplate (kitchenCabinetAboveSinkGID, kitchenSinkGID)
import Data.These (These(..))
import HauntedHouse.Game.Model.Condition 
        (Moveability(..), Perceptibility (..), Proximity (PlacedAbove))
import HauntedHouse.Game.Actions.Close (standardCloseM)
import HauntedHouse.Game.Actions.Open (standardOpenM)
import HauntedHouse.Game.Actions.Get (noGetM)
import HauntedHouse.Tokenizer (Lexeme (CABINET))
import HauntedHouse.Game.Actions.Look.StandardLook (lookIn, lookAt, lookWrapper)
import Control.Monad.Except (MonadError(..))

buildKitchenCabinetAboveSink :: GameStateExceptT ()
buildKitchenCabinetAboveSink = do
  world <- _world' <$> get 
  let objectMap' :: GIDToDataMapping Object 
      objectMap' = 
        GIDToDataMapping 
          $ Data.Map.Strict.insert 
              kitchenCabinetAboveSinkGID buildCabinet 
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
    , _mNexus' =  (Just . Containment') cabinetContainer
    , _standardActions' = standardActions
  }
  where 
    desc = "You can put things in it."

standardActions :: StandardActions
standardActions = StandardActions 
  { _get' = const pass -- noGetM
  , _put' = const pass 
  , _lookIn' = lookWrapper lookIn
  , _lookAt' = lookAt
  , _lookOn' = const $ throwError ("There's nothing of interest" :: Text)
  }

orientation :: Orientation 
orientation = AnchoredTo' (kitchenSinkGID, PlacedAbove)
cabinetContainer :: Containment
cabinetContainer = (Containment . This) containedIn

containedIn :: ContainedIn
containedIn = ContainedIn 
  {_containerInterface' = containerInterface
  , _containedIn' = ContainerMap Data.Map.Strict.empty  
  }

containerInterface :: ContainerInterface 
containerInterface = ContainerInterface {
      _describe' = mempty
    , _openState'     = Open 
    , _openAction'    = standardOpenM kitchenCabinetAboveSinkGID
    , _closeAction'   = standardCloseM kitchenCabinetAboveSinkGID
    , _lockAction'    = pass
    , _unlockAction'  = pass
  }
