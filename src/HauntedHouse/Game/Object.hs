module HauntedHouse.Game.Object where


getObjectMap :: GameStateExceptT ObjectMap
getObjectMap = _objectMap <$> get