module Build.Locations.Kitchen.PlantPot where
import Build.ObjectTemplate
import Game.Model.Mapping 
        (GIDToDataMapping (..), ContainerMap (..), Label (..))
import qualified Data.Map.Strict
import Game.Model.Condition (Moveability(..), Perceptibility (..))
import Data.These (These(..))
import Game.Actions.Get (tryGetM, standardGetM)
import Tokenizer.Data ( Lexeme(PLANT, SMALL, POT) )
import Game.Actions.Look.PlantPot ( firstLook, lookOn)
import Game.Model.World 
import Game.Object (getObjectM)
import Build.ObjectTemplate (plantPotGID)
import Game.Model.Display (updateEnvironmentM)
import Control.Monad.RWS (MonadWriter())

buildPlantPot :: GameStateExceptT ()
buildPlantPot = do
  world <- _world' <$> get
  let objectMap' :: GIDToDataMapping Object
      objectMap' =
        GIDToDataMapping $ Data.Map.Strict.insert plantPotGID plantPot
          $ (_unGIDToDataMapping' . _objectMap') world
  modify' (\gs -> gs{_world' = world{_objectMap' = objectMap'}})


plantPot :: Object
plantPot = Object {
      _shortName'      = "plant pot"
    , _entityLabel' = Label POT
    , _odescription'   = [desc]
    , _descriptives'   = [Label PLANT, Label SMALL]
    , _moveability'    = NotMoveable
    , _perceptability' = Perceptible
    , _orientation'    = floorOrientation -- orientation 
    , _standardActions' = standardActions
  }
  where
    desc = "You can plant plants in the plant pot."

standardActions :: StandardActions
standardActions = StandardActions {
    _getAction' = getAction 
  , _putAction' = putAction
  , _lookAction' = lookAction
  , _openAction' = openAction 
  , _closeAction' = closeAction
  , _lockAction' = lockAction
  , _unlockAction' = unlockAction
  , _goAction' = goAction
}

goAction :: GoAction 
goAction = GoAction 
  { _updateGo' = pass 
  , _go' = pass 
  }
unlockAction :: UnlockAction 
unlockAction = UnlockAction 
  { _updateUnlock' = pass 
  , _unlock' = unlock 
  }

unlock :: GameStateExceptT () 
unlock = updateEnvironmentM msg 
  where
    msg = "You don't need me to tell you that"
            <> " you can't unlock something that" 
            <> " can't be locked in the first place."

getAction :: GetAction 
getAction = GetAction 
  {   _updateGet' = pass 
    , _get' = pass -- tryGetM plantPotGID standardGetM
  }

putAction :: PutAction 
putAction = PutAction 
  {   _updatePut' = pass
    , _put' = const (const pass)
  }

lookAction :: LookAction 
lookAction = LookAction 
  {   _updateLook' = updateLookToStageTwo 
    , _look' = const (const lookStageOne) 
  }

updateLookToStageTwo :: GameStateExceptT () 
updateLookToStageTwo = pass 

updateLookToStageThree :: GameStateExceptT ()
updateLookToStageThree = pass 

lookAtStageOne :: GameStateExceptT ()
lookAtStageOne = updateEnvironmentM msg 
  where 
    msg = "This plant pot is empty. "
            <> "But if you put some potting soil in it, "
            <> "and planted a plant, you might get somewhere. "
            <> "Like finishing the demo, for example"

lookAtStageTwo :: GameStateExceptT () 
lookAtStageTwo = updateEnvironmentM msg 
  where
    msg = "This plant pot has some potting soil in it"
            <> "If you planted some sort of plant in the plant pot, "
            <> "you will have completed the demo"

lookAtStageThree :: GameStateExceptT () 
lookAtStageThree = updateEnvironmentM msg 
  where 
    msg = "Plant pot and pot plant stand united"
            <> "Magnetic Scrolls did it first"

open :: GameStateExceptT ()
open = do 
  updateEnvironmentM msg 
  where 
    msg = "Very funny, but we both know you can't open a plant pot"

openAction :: OpenAction
openAction = OpenAction
  { _updateOpen' = pass 
  , _open' = const open
  }

closeAction :: CloseAction 
closeAction = CloseAction 
  { _updateClose' = pass
  , _close' = close 
  }

close :: GameStateExceptT () 
close = 
  updateEnvironmentM msg 
  where
    msg = "Having fun with the parser, I see. You can't close a plant pot."

lockAction :: GameStateExceptT () 
lockAction = LockAction 
  { _updateLock' = pass
  , _lock' = lock
  }

lock :: GameStateExceptT ()
lock = updateEnvironmentM msg 
  where
    msg = "That would be a neat trick, but you can't lock a plant pot."
floorOrientation :: Orientation
floorOrientation = Floor kitchenFloorGID
