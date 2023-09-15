module HauntedHouse.Game.Player where

import HauntedHouse.Game.Model.GID
import HauntedHouse.Game.Model.World
        (Player (..), Location, GameStateExceptT, GameState (..), Object (..), Nexus, Orientation (..))
import qualified Data.List.NonEmpty (insert, singleton)
import HauntedHouse.Game.Object (setObjectMapM)

setPlayerLocationM :: GID Location -> GameStateExceptT ()
setPlayerLocationM gid = do
  player <- _player' <$> get
  modify' (\gs -> gs{_player' = player{_playerLocation' = gid}})
  pass
-- fromGid fromEntity updatedNexus
-- FIXME: all add/remove happens here. 
setPlayerInventoryM :: GID Object 
                        -> Object
                        -> GID Object 
                        -> Object
                        -> Nexus  
                        -> GameStateExceptT ()
setPlayerInventoryM newEntityGid newEntity fromGid fromEntity updatedNexus = do
  player <- _player' <$> get
  let updatedInv = case _p_inv' player of
                    []      -> [newEntityGid]
                    invList -> newEntityGid : invList
      updatedPlayer = player{_p_inv' = updatedInv}
  setObjectMapM fromGid (fromEntity{_mNexus' = Just updatedNexus})
  setObjectMapM newEntityGid (newEntity{_orientation' = Inventory})
  modify' (\gs -> gs{_player' = updatedPlayer})
  pass
