module Build.Locations.Kitchen.SinkArea.Sink where

import Data.Map.Strict qualified

import Build.ObjectTemplate
import Game.Model.Mapping
import Game.Model.World
import Game.Model.Condition
        (Perceptibility(Perceptible), Moveability (..))
import Game.World 
import Tokenizer (Lexeme (SINK)) 
import Build.Locations.Kitchen.SinkArea.Actions.Put
import Build.Locations.Kitchen.SinkArea.Actions.Look 
import Build.Locations.Kitchen.SinkArea.Actions.Open (openAction)
import Build.Locations.Kitchen.SinkArea.Actions.Close (closeAction)
import Build.Locations.Kitchen.SinkArea.Actions.NoCanDo

buildKitchenSink :: GameStateExceptT ()
buildKitchenSink = do
  world <- _world' <$> get
  let objectMap' :: GIDToDataMapping Object Object
      objectMap' =
        GIDToDataMapping $ Data.Map.Strict.insert kitchenSinkGID buildSink
          $ (_unGIDToDataMapping' . _objectMap') world
  initContainerMapM kitchenSinkGID sinkContainer
  modify' (\gs -> gs{_world' = world{_objectMap' = objectMap'}})

buildSink :: Object
buildSink = Object {
    _shortName'     = "kitchen sink."
  , _entityLabel' = Label SINK
  , _odescription'  = [desc]
  , _descriptives' = []
  , _moveability'   = NotMoveable
  , _perceptability' = Perceptible
  , _orientation' = orientation

  , _standardActions' = standardActions
  }
  where
    desc = "This sink is broken. You can put things in it."

standardActions :: StandardActions
standardActions = StandardActions { 
    _getAction' = getAction
  , _putAction' = putAction 
  , _lookAction' = lookAction
  , _openAction' = openAction
  , _closeAction' = closeAction 
  , _lockAction' = lockAction
  , _unlockAction' = unlockAction
  , _goAction' = goAction
}

orientation :: Orientation
orientation = Anchored EastAnchor

sinkContainer :: Container
sinkContainer = Container 
  $ ContainerMap Data.Map.Strict.empty