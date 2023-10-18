module Build.Locations.Kitchen.SinkArea.Sink where

import Data.Map.Strict qualified
import Build.ObjectTemplate ( kitchenSinkGID )
import Game.Model.Mapping
import Game.Model.World
import Game.World 
import Tokenizer (Lexeme (SINK)) 
import Build.Locations.Kitchen.SinkArea.Actions.Put
import Build.Locations.Kitchen.SinkArea.Actions.Look 
import Build.Locations.Kitchen.SinkArea.Actions.Open (openAction)
import Build.Locations.Kitchen.SinkArea.Actions.Close (closeAction)
import Build.Locations.Kitchen.SinkArea.Actions.NoCanDo
import Game.Object (setObjectMapM, getAnchoreds)
import Build.LocationTemplate (kitchenGID)

buildKitchenSink :: GameStateExceptT ()
buildKitchenSink = do
  setObjectMapM kitchenSinkGID buildSink
  initContainerMapM kitchenSinkGID sinkContainer

buildSink :: Object
buildSink = Object {
    _shortName'     = "kitchen sink."
  , _entityLabel' = Label SINK
  , _odescription'  = [desc]
  , _descriptives' = []
  , _orientation' = orientation
  , _standardActions' = standardActions
  }
  where
    desc = "This sink is broken. You can put things in it."

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
orientation = Anchor sinkAnchor

sinkAnchor :: GameStateExceptT (Maybe (NonEmpty Anchored))
sinkAnchor = getAnchoreds kitchenGID EastSection kitchenSinkGID notAnchorMsg
  where
    notAnchorMsg = show kitchenSinkGID <> " is not an anchor"

sinkContainer :: Container
sinkContainer = Container Data.Map.Strict.empty