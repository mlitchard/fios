module HauntedHouse.Game.Player where 
  
import HauntedHouse.Game.Model.GID
import HauntedHouse.Game.Model.World
        (Player (..), Location, GameStateExceptT, GameState (..), Object)
import qualified Data.List.NonEmpty (insert)

setPlayerLocationM :: GID Location -> GameStateExceptT ()
setPlayerLocationM gid = do 
  player <- _player' <$> get
  modify' (\gs -> gs{_player' = player{_playerLocation' = gid}})
  pass
-- , _p_inv'           :: Maybe (Data.List.NonEmpty.NonEmpty (GID Object))
setPlayerInventoryM :: GID Object -> GameStateExceptT ()
setPlayerInventoryM gid = do 
  player <- _player' <$> get
  let updatedInv = Data.List.NonEmpty.insert gid <$> _p_inv' player
      updatedPlayer = player{_p_inv' = updatedInv}
  modify' (\gs -> gs{_player' = updatedPlayer})
  pass

