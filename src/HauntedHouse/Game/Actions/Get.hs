module HauntedHouse.Game.Actions.Get where
import HauntedHouse.Game.Model.World
import Control.Monad.Except (MonadError(..))
import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Game.Object (getObjectM, setObjectMapM)
import Prelude hiding (fromLabel)
import HauntedHouse.Game.Engine.Utilities
import Data.These
import HauntedHouse.Game.Player (setPlayerInventoryM)
import HauntedHouse.Recognizer (AdjPhrase)
import HauntedHouse.Game.Engine.Verification (verifyAccessabilityAP)

noGetM :: GameStateExceptT ()
noGetM = throwError "Are you serious?"

tryGetM :: GID Object -> Object -> GameStateExceptT ()
tryGetM gid entity@(Object{..}) = do 
  let getInput onOrIn = 
        GetInput { 
          _removedEntityGID' = gid
          , _removedEntity' = entity
          ,_removedEntityLabel' = _entityLabel' 
          ,_entityOnOrIn' = onOrIn 
        }  
  case _orientation' of
    ContainedBy' (ContainedBy cBy _) -> standardGetM (getInput cBy)
    Inventory -> throwError alreadyHaveMSG
    Floor fromGid -> standardGetM (getInput (On fromGid))
    AnchoredTo' (fromGid, _) -> standardGetM (getInput (On fromGid))
    Anchoring _ -> throwError sillyMSG
  where
    alreadyHaveMSG = "Neat trick, trying tp get something you already have."
    sillyMSG = "You begin to feel a little silly trying to get that."

standardGetM :: GetInput
                  -> GameStateExceptT ()
standardGetM (GetInput rGid _removedEntity' rLabel (On fromGid)) = do
  fromObject@(Object {..}) <- getObjectM fromGid
  nexus <- throwMaybeM notContainerMSG _mNexus'
  containment <- throwMaybeM notContainerMSG (maybeContainment nexus)
  nexus' <- Containment' . Containment <$> case _unContainment' containment of
    This _ -> throwError impassMSG
    That con -> do
                  updatedCon <- removeFromContainedOnM rGid rLabel con
                  pure (That updatedCon)
    These cin con -> do
                       updatedCon <- removeFromContainedOnM rGid rLabel con
                       pure (These cin updatedCon)
  setObjectMapM fromGid (fromObject{_mNexus' = Just nexus'})
  setPlayerInventoryM rGid
  setObjectMapM rGid (_removedEntity'{_orientation' = Inventory})
  where
    notContainerMSG = "standardGetM' error: fromObject not a container"
    impassMSG = "standardGetM: illogical situation "

standardGetM (GetInput rGid _removedEntity' rLabel (In fromGid)) = do 
  fromObject@(Object {..}) <- getObjectM fromGid
  nexus <- throwMaybeM notContainerMSG _mNexus'
  containment <- throwMaybeM notContainerMSG (maybeContainment nexus)
  nexus' <- Containment' . Containment <$> case _unContainment' containment of
    This cin -> do 
                updatedCin <- removeFromContainedInM rGid rLabel cin
                pure (This updatedCin) -- throwError impassMSG
    That _ -> throwError impassMSG 
    These cin con -> do
                       updatedCin <- removeFromContainedInM rGid rLabel cin
                       pure (These updatedCin con)
  setObjectMapM fromGid (fromObject{_mNexus' = Just nexus'})
  setPlayerInventoryM rGid
  setObjectMapM rGid (_removedEntity'{_orientation' = Inventory})
  where
    notContainerMSG = "standardGetM' error: fromObject not a container"
    impassMSG = "standardGetM: illogical situation "

{-
standardGetM :: GID Object -- entity gotten
                  -> GID Object -- entity gotten from

                  -> GameStateExceptT ()
standardGetM removedGID fromGID = do
  fromEntity <- getObjectM fromGID
  removedEntity <- getObjectM removedGID
  let labelRemovedEntity = _entityLabel' removedEntity
      removeFunctionIn = removeFromContainedInM removedGID labelRemovedEntity
      removeFunctionOn = removeFromContainedOnM removedGID labelRemovedEntity
  nexus <- throwMaybeM notContainerMSG (_mNexus' fromEntity)
  containment <- throwMaybeM notContainerMSG (maybeContainment nexus)
  onOrIn <- throwMaybeM notContainedMSG (toOnOrIn (_orientation' removedEntity))
  let unContainment = _unContainment' containment
  res <- throwMaybeM impassMSG =<< (case unContainment of
    (This cIn) -> case onOrIn of
                    (On _) -> pure Nothing
                    (In _) -> Just . This <$> removeFunctionIn cIn
    (That cOn) -> case onOrIn of
                    (On _) -> Just . That <$> removeFunctionOn cOn
                    (In _) -> pure Nothing
    (These cIn cOn) -> case onOrIn of
                        (On _) -> Just . That <$> removeFunctionOn cOn
                        (In _) -> Just . This <$> removeFunctionIn cIn)
  let updatedNexus = (Containment' . Containment) res
      updatedFromEntity = fromEntity {_mNexus' = Just updatedNexus}
  setObjectMapM fromGID updatedFromEntity
  -- FIXME change orientation of gotten object to Inventory
  where
    impassMSG = "standardGetM: illogical situation "
    notContainedMSG = "Not contained"
    notContainerMSG = "Not a container"
-}
