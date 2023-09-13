module HauntedHouse.Build.Default where

import Data.List.NonEmpty qualified             (singleton
                                                )
import Data.Map.Strict qualified                (empty)
import Data.Text qualified                      (empty)

import HauntedHouse.Build.LocationTemplate
import HauntedHouse.Game.Model.World

import HauntedHouse.Game.Model.Mapping
    (GIDToDataMapping (..), LabelToGIDListMapping (..),)
import HauntedHouse.Game.Narration (displaySceneM, makeSceneM)
import HauntedHouse.Game.Engine (primaryEvaluator)
import HauntedHouse.Clarifier (clarifyWhich)
import HauntedHouse.Game.Engine.VerbPhraseOneEvaluator (evalVerbNounPhrase)
import HauntedHouse.Game.Engine.VerbPhraseTwo (evalVerbPrepPhrase)
import HauntedHouse.Game.Engine.VerbPhraseThree (verbPhraseThree)
import HauntedHouse.Game.Engine.VerbPhraseSeven 
import HauntedHouse.Game.Engine.VerbPhraseFive (verbPhraseFive)
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
{-
data Config = Config {
  _primaryEvaluator'         :: Imperative -> GameStateExceptT () 
  , _clarifyWhich'           :: ClarifyWhich                          
  , _evalVerbNounPhrase'     :: (Verb, NounPhrase) -> GameStateExceptT ()
  , _evalVerbPrepPhrase'     :: (Verb, PrepPhrase) -> GameStateExceptT ()
  , _evalVerbTwoPrepPhrases' :: EvalVerbThree                                      
}
-}
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

defaultWorld :: World
defaultWorld = World
  { _objectMap'     = GIDToDataMapping Data.Map.Strict.empty
    , _locationMap' = GIDToDataMapping Data.Map.Strict.empty
    , _descriptiveMap' = LabelToGIDListMapping Data.Map.Strict.empty
    , _exitMap'     = GIDToDataMapping Data.Map.Strict.empty
  }

defaultPlayer :: Player
defaultPlayer = Player
  {_playerLocation' = kitchenGID
    , _p_inv'         = Nothing
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

defaultRoomAnchors :: RoomAnchors
defaultRoomAnchors = RoomAnchors Data.Map.Strict.empty