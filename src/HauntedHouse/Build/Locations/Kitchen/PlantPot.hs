module HauntedHouse.Build.Locations.Kitchen.PlantPot where 
import HauntedHouse.Game.Model.World (GameStateExceptT)
import HauntedHouse.Build.ObjectTemplate 

buildPlantPot :: GameStateExceptT ()
buildPlantPot = do
  world <- _world' <$> get
  let objectMap' :: GIDToDataMapping Object 
      objectMap' = 
        GIDToDataMapping $ Data.Map.Strict.insert plantPotGID plantPot 
          $ (_unGIDToDataMapping' . _objectMap') world
  modify' (\gs -> gs{_world' = world{_objectMap' = objectMap'}})  
  pass

plantPot :: Object 
plantPot = Object {
    _shortName'     = "A kitchen sink."
  , _odescription'  = [desc]
  , _descriptives' = []
  , _moveability'   = NotMoveable
  , _perceptability' = Perceptible
  , _mNexus'         = Nothing
  }
  where 
    desc = "This sink is broken. You can put things in it."
