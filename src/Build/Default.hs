module Build.Default where

import Data.List.NonEmpty qualified             (singleton
                                                )
import Data.Map.Strict qualified                (empty)
import Data.Text qualified                      (empty)

import Build.LocationTemplate
import Game.Model.World

import Game.Model.Mapping
    (GIDToDataMap (..), LabelToGIDListMapping (..), GIDToGIDMapping (..),)
import Game.Narration (displaySceneM, makeSceneM)
import Game.Engine (primaryEvaluator)
import Clarifier (clarifyWhich)
import Game.Engine.VerbPhraseOneEvaluator (evalVerbNounPhrase)
import Game.Engine.VerbPhraseTwo (evalVerbPrepPhrase)
import Game.Engine.VerbPhraseThree (verbPhraseThree)
import Game.Engine.VerbPhraseSeven 
import Game.Engine.VerbPhraseFive (verbPhraseFive)
import Game.Model.Display (showEnvironmentM)
defaultGameState :: GameState
defaultGameState = GameState
  { _world' = defaultWorld
  , _report' = []
  , _player' = defaultPlayer
  , _narration' = defaultNarration
  , _verbosity' = Loud
  , _displayAction' = defaultDisplayAction
  , _clarification' = Nothing
  , _clarifiedDirectObject'  = Nothing
  , _evaluator' = primaryEvaluator
  }

config :: Config 
config = Config {
  _primaryEvaluator'     = primaryEvaluator
  , _clarifyWhich'       = clarifyWhich
  , _evalVerbNounPhrase' = evalVerbNounPhrase 
  , _evalVerbPrepPhrase' = evalVerbPrepPhrase   
  , _evalVerbTwoPrepPhrases' = verbPhraseThree 
  , _evalVerbPhraseSeven' = verbPhraseSeven
  , _evalVerbPhraseFive' = verbPhraseFive 
}
defaultDisplayAction :: GameStateExceptT ()
defaultDisplayAction = 
  report 
    >> getLocationIdM 
    >>= getLocationM 
    >>= makeSceneM 
    >> displaySceneM True
    >> showEnvironmentM

defaultWorld :: World
defaultWorld = World
  { _objectMap'     = GIDToDataMap Data.Map.Strict.empty
    , _locationMap' = GIDToDataMap Data.Map.Strict.empty
    , _containerMap' = GIDToDataMap Data.Map.Strict.empty
    , _descriptiveMap' = LabelToGIDListMapping Data.Map.Strict.empty
    , _exitMap'     = GIDToGIDMapping Data.Map.Strict.empty
  }


defaultPlayer :: Player
defaultPlayer = Player
  {_playerLocation' = kitchenGID
    , _p_inv'         = mempty 
  }

defaultNarration :: Narration
defaultNarration = Narration
  {_playerAction' = Data.List.NonEmpty.singleton Data.Text.empty
  , _enviroment' = Data.List.NonEmpty.singleton Data.Text.empty
  , _npcResponse' = Data.List.NonEmpty.singleton Data.Text.empty
  , _scene' = defaultScene
  }

defaultScene :: Scene
defaultScene = Scene
  { _sceneTitle' = Data.Text.empty
  , _sceneDescription' = Data.Text.empty
  , _roomAnchored' = mempty
  , _visibleExits' = mempty
  }

defaultObjectLabelMap :: LabelToGIDListMapping Object Object
defaultObjectLabelMap = LabelToGIDListMapping Data.Map.Strict.empty

defaultRoomSections :: RoomSectionMap
defaultRoomSections = Data.Map.Strict.empty