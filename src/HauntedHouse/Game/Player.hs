module HauntedHouse.Game.Player where 
  
import HauntedHouse.Game.Model.GID
import HauntedHouse.Game.Model.World 
        (Player (..), Location, GameStateExceptT, GameState (..))

setPlayerLocationM :: GID Location -> GameStateExceptT ()
setPlayerLocationM gid = do 
  player <- _player' <$> get
  modify' (\gs -> gs{_player' = player{_playerLocation' = gid}})
  pass
