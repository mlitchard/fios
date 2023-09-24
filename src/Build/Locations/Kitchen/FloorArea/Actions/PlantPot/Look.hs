module Build.Locations.Kitchen.FloorArea.Actions.PlantPot.Look where
import Game.Model.World
        (LookAction (..), GameStateExceptT, Object, LookAtF (..)
        , LookInF (..), LookOnF (..))
import Game.Model.Display (updateEnvironmentM)
import Game.Actions.Look.StandardLook (makeLookAction, changeLookAction)
import Build.ObjectTemplate (plantPotGID)
import Game.Object (setObjectMapM)


updateLookActionObject :: Object -> GameStateExceptT ()
updateLookActionObject = setObjectMapM plantPotGID

hasPlantLookAction :: LookAction
hasPlantLookAction =
  makeLookAction (const pass) lookAtHasPlantF lookOnF lookInHasPlantF

-- lookAtHasPlantF :: LookAtF 
lookAtHasPlantF :: LookAtF
lookAtHasPlantF = LookAtF lookHasPlant

lookInHasPlantF :: LookInF 
lookInHasPlantF = LookInF lookHasPlant 

lookOnF :: LookOnF 
lookOnF = LookOnF $ const (const (updateEnvironmentM msg)) 
  where 
    msg = "You see an etching that looks suspiciously like instructions"
            <> "Put soil in plant pot"
            <> "Plant pot plant in plant pot with trowel"
            <> "Alternative second step: Use trowel to plant pot plant in plant pot."

lookHasPlant :: a -> b -> GameStateExceptT ()
lookHasPlant = const (const (updateEnvironmentM msg))
  where
    msg = "Pot plant and plant pot have become one. "
            <> "You've completed this demo."
            <> "Magnetic Scrolls did it first."

emptyPlantPotLookAction :: LookAction
emptyPlantPotLookAction =
  makeLookAction changeToHasSoilLook lookAtEmptyF lookOnF lookInEmptyF

lookInEmptyF :: LookInF 
lookInEmptyF = LookInF lookEmpty 

lookAtEmptyF :: LookAtF
lookAtEmptyF = LookAtF lookEmpty 

lookEmpty :: a -> b -> GameStateExceptT ()
lookEmpty = const (const (updateEnvironmentM msg))
  where 
    msg = "This empty pot plant could use some soil in it. "
            <> "To plant a plant,perhaps."

hasSoilLookAction :: LookAction
hasSoilLookAction =
  makeLookAction changeToPlantedLook lookAtHasSoilF lookOnF lookInHasSoilF

lookAtHasSoilF :: LookAtF 
lookAtHasSoilF = LookAtF lookHasSoil 

lookInHasSoilF :: LookInF 
lookInHasSoilF = LookInF lookHasSoil 

lookHasSoil :: a -> b -> GameStateExceptT ()
lookHasSoil = const (const (updateEnvironmentM msg))
  where
    msg = "The plant has soil and is now ready for a plant of some "
            <> "kind to be planted in it."

changeToHasSoilLook :: Object -> GameStateExceptT ()
changeToHasSoilLook entity =
  updateLookActionObject $ changeLookAction hasSoilLookAction entity

changeToPlantedLook :: Object -> GameStateExceptT ()
changeToPlantedLook entity =
  updateLookActionObject $ changeLookAction hasPlantLookAction entity
