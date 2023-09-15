module HauntedHouse.Game.Actions.Look where
import HauntedHouse.Game.Model.World
import HauntedHouse.Game.Model.Display (openSeeDeep)
import Data.These
import Control.Monad.Except (MonadError(..))

{-
data ContainedIn = ContainedIn
  { _containerInterface'  :: ContainerInterface
  , _containedIn'         :: ContainerMap Object
  }

  data ContainerInterface = ContainerInterface {
      _openState'    :: OpenState
    , _describe'      :: Text 
    , _openAction'   :: GameStateExceptT ()
    , _closeAction'  :: GameStateExceptT ()
    , _lockAction'   :: GameStateExceptT ()
    , _unlockAction' :: GameStateExceptT ()
  }
-}
-- (Containment containment)
lookIn :: Object  -> GameStateExceptT ()
lookIn  (Object {..}) = do
  (Containment containment) <- getContainmentM mNexus 
  let (openState, cIn) = case containment of 
    (This (ContainedIn( (ContainerInterface{..} cIn))) -> (_openState',cIn) 
    (That _) -> throwError "err"
    (These ((ContainedIn( (ContainerInterface{..} cIn)))) _) -> (_openState',cIn)
  closedCheckM _openState' 
     >> makeDescriptionListM cIn
     >> makeDescriptionListM containedIn 
     >>= openSeeDeep
     >>= updateEnvironmentM
-- where 
--    programmerErr = "lookIn: Programmer Error can't look on with lookIn"
{-
closedCheckM _openState' 
                                  >> makeDescriptionListM containedIn 
                                  >>= openSeeDeep
-}
closedCheckM :: OpenState -> GameStateExceptT () 
closedCheckM Open = pass 
closedCheckM Closed = "You can't look inside, because it's closed."