module HauntedHouse.Game.Agent (
  module HauntedHouse.Game.Agent
, module HauntedHouse.Game.Agent.Domain
, module HauntedHouse.Game.Agent.Atomic 
)where

import Data.Map.Strict ( lookup ) 
import HauntedHouse.Game.Agent.Domain
import HauntedHouse.Game.Agent.Atomic
import Control.Monad.Except ( MonadError(throwError) )
import HauntedHouse.Game.GameState.Domain
    ( GameStateExceptT, GameState(..) )
import HauntedHouse.Game.GID (GID)

getAgentData :: GameStateExceptT AgentData
getAgentData = do
    playerName <- getPlayer
    gs <- get 
    let aMap = _agentMap gs 
        mAgent = lookup playerName $ unAgentMap aMap
    case mAgent of 
        (Just ad) -> pure ad 
        Nothing   -> throwError ("can't find " <> show playerName ) 

getPlayer :: GameStateExceptT (GID AgentName)
getPlayer = _player <$> get