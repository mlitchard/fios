module HauntedHouse.Game.Engine.OnlyVerb.DoLook where
import HauntedHouse.Game.Model (GameStateExceptT, GameState (_narration'))
import HauntedHouse.Game.Model (Narration (..))

doLook :: GameStateExceptT ()
doLook = do
  scene <- _scene' . _narration' <$> get
  print scene


