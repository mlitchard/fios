module HauntedHouse.Game.Player where 
  
import HauntedHouse.Game.Model.GID
import HauntedHouse.Game.Model.World
        (Player (..), Location, GameStateExceptT, GameState (..), Object)
import qualified Data.List.NonEmpty (insert, singleton)

setPlayerLocationM :: GID Location -> GameStateExceptT ()
setPlayerLocationM gid = do 
  player <- _player' <$> get
  modify' (\gs -> gs{_player' = player{_playerLocation' = gid}})
  pass

-- FIXME: all add/remove happens here. 
setPlayerInventoryM :: GID Object -> GameStateExceptT ()
setPlayerInventoryM gid = do 
  player <- _player' <$> get
  let updatedInv = case _p_inv' player of 
                    Nothing -> Data.List.NonEmpty.singleton gid
                    Just invList -> Data.List.NonEmpty.insert gid invList 
      updatedPlayer = player{_p_inv' = Just updatedInv}
  modify' (\gs -> gs{_player' = updatedPlayer})
  pass

