module HauntedHouse.Game.Narration (
  module HauntedHouse.Game.Narration
, HauntedHouse.Game.Narration.Domain.Narration (..))
where 
import HauntedHouse.Game.Narration.Domain (Narration (..))
import HauntedHouse.Game.GameState (GameStateExceptT)
import HauntedHouse.Game.GameState.Domain (GameState(_narration))

updateNarration :: Narration -> GameStateExceptT ()
updateNarration narration =  modify' (\g -> g {_narration = narration})
