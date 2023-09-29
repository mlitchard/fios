module Game.Actions.Look.Update where 
import Game.Model.World
        (LookF, LookAction (..), UpdatePerceptionFunctions (..)
        , PerceptionFunctions (..), LookFunctions (..), LookAtF (..)
        , GameStateExceptT, Object, LookInF, LookOnF )

lookPerceptionF :: (LookF -> LookF) -> LookAction -> LookAction 
lookPerceptionF f lookAction =
  let perception = _perception' lookAction 
  in lookAction {_perception' = perception{_lookPerceptionF' = f}}

updateDisplayPerceptionF :: (Text -> Maybe Text) -> LookAction -> LookAction 
updateDisplayPerceptionF f lookAction = 
  let perception = _perception' lookAction 
  in lookAction {_perception' = perception{_displayPerceptionF' = f}}

updateOpenReport :: (Object -> GameStateExceptT ()) -> LookAction -> LookAction
updateOpenReport f lookAction = 
  let updatePerceptionFunctions = _updatePerception' lookAction
      updatePerception = updatePerceptionFunctions{_updateBlockReport'= f}
  in lookAction {_updatePerception' = updatePerception}

updateVisibility :: (Object -> GameStateExceptT ()) 
                      -> LookAction 
                      -> LookAction
updateVisibility f lookAction = 
  let updatePerceptionFunctions = _updatePerception' lookAction
      updatePerception = updatePerceptionFunctions {_updateDisplay' = f}
  in lookAction {_updatePerception' = updatePerception}

updateLookAtF :: LookAtF -> LookAction -> LookAction 
updateLookAtF f lookAction =
  let lookFunctions = _lookFunctions' lookAction 
  in lookAction {_lookFunctions' = lookFunctions {_lookAt' = f}} 

updateLookInF :: LookInF -> LookAction -> LookAction
updateLookInF f lookAction = 
  let lookFunctions = _lookFunctions' lookAction 
  in lookAction {_lookFunctions' = lookFunctions {_lookIn' = f}} 

updateLookOnF :: LookOnF -> LookAction -> LookAction 
updateLookOnF f lookAction =
  let lookFunctions = _lookFunctions' lookAction 
  in lookAction {_lookFunctions' = lookFunctions {_lookOn' = f}}