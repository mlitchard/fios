module HauntedHouse.Game.Object where
import HauntedHouse.Game.Object.Domain (ObjectMap)
import HauntedHouse.Game.GameState

getObjectMap :: GameStateExceptT ObjectMap
getObjectMap = _objectMap <$> get