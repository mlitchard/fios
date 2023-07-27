module HauntedHouse.Game.Engine.OnlyVerb.DoLook where
import HauntedHouse.Game.GameState (GameStateExceptT, GameState (_narration'))
import HauntedHouse.Game.Narration (Narration (..))

doLook :: GameStateExceptT ()
doLook = do
  scene <- _scene' . _narration' <$> get
  print scene


