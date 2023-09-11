module HauntedHouse.Build.Actions.Open where
import HauntedHouse.Game.Model.World
import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Game.Object (getObjectM, getContainerInterfaceM)
import Control.Monad.Except (MonadError(..))
import Data.These (These(..))

standardOpenM :: GID Object -> GameStateExceptT ()
standardOpenM gid = do
  entity <- getObjectM gid
  containment <- throwRightM "not" . _unNexus' =<< throwMaybeM notContainerMSG (_mNexus' entity)
  case _unContainment' containment of
    (This containerIn) -> pass
    (That _) -> throwError "How did a portal get this function?"
    (These containerIn containerOn ) -> pass

  -- let nexus = ContainerInterface' updatedContainerInterface
  pass
  where
    notContainerMSG = "standardOpen error: not a nexus"