module HauntedHouse.Game.Actions.Look where

import HauntedHouse.Game.Model.World
import HauntedHouse.Game.World
import Data.These
import HauntedHouse.Game.Model.Mapping
import HauntedHouse.Game.Model.Display
import qualified Data.Map.Strict
import HauntedHouse.Game.Object
import qualified Data.List.NonEmpty
import qualified Data.List
import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Game.Model.Condition (Perceptibility(Perceptible))
import Control.Monad.Except (MonadError(..))
import HauntedHouse.Game.Engine.Verification (containerTest)

lookWrapper :: (Text -> Containment  -> GameStateExceptT ())
                -> Object
                -> GameStateExceptT ()
lookWrapper lookFunction (Object{..}) = do
  container <-  throwMaybeM wrongFunction $ containerTest =<< _mNexus'
  lookFunction _shortName' container
  where
    wrongFunction = "lookWrapper: This object has the wrong look function"
                      <> _shortName'
lookIn :: Text -> Containment  -> GameStateExceptT ()
lookIn shortName containment = do
  (openState, cIn) <- throwMaybeM notMsg (caseContainmentIn containment)
  closedCheckM openState shortName cIn openSeeDeep -- >> makeDescriptionListM cIn
  where
    notMsg = "lookIn: Can't look in one of those"

lookOn :: Text -> Containment -> GameStateExceptT ()
lookOn shortname containment = do
  cmap <- throwMaybeM noMsg $ caseContainmentOn containment
  res <- concatMap (Data.List.NonEmpty.toList . snd) <$> filterPerceptiblesM cmap
  updateEnvironmentM ("You see the following on the " <> shortname)
  mapM_ updateEnvironmentM =<< mapM getShortNameM res
   where
    noMsg = "You can't look on this " <> shortname

lookAt :: Object -> GameStateExceptT ()
lookAt (Object{..}) = do
  case _mNexus' of
    (Just (Containment' containment)) -> do
                                          updatePlayerActionM lookMsg
                                            >> mapM_ updateEnvironmentM _odescription'
                                            >> displayPossibleContained _shortName' (caseContainmentAt containment)

    _ -> do
          if _perceptability' == Perceptible
            then success
            else throwError "You don't see that."
  where
    lookMsg = "You look at the " <> _shortName'
    success = updatePlayerActionM ("You look at the " <> _shortName')
                >> mapM_ updateEnvironmentM _odescription'

data AllObjects = AllObjects
  { _objectsIn' :: (OpenState, ContainerMap Object)
  , _objectsOn' :: ContainerMap Object
  }
data PossibleContained
  = AllObjects' AllObjects
  | ObjectsIn (OpenState, ContainerMap Object)
  | ObjectsOn (ContainerMap Object)
-- capturePerceptibleM :: GIDList Object
                     --   -> GameStateExceptT (Maybe (NonEmpty (GID Object,Object)))
{-
newtype ContainerMap a = ContainerMap 
  { _unContainerMap' :: Map (Label a) (GIDList a)} 
-}
displayPossibleContained :: Text -> PossibleContained -> GameStateExceptT ()
displayPossibleContained shortname (AllObjects' AllObjects{..}) = do
  closedCheckM (fst _objectsIn') shortname (snd _objectsIn') openSeeShallow
  res <- concatMap (Data.List.NonEmpty.toList . snd)
          <$> filterPerceptiblesM _objectsOn'
  updateEnvironmentM ("You see the following on the " <> shortname)
  mapM_ updateEnvironmentM =<< mapM getShortNameM res

displayPossibleContained shortname (ObjectsIn (openState, cmap)) = do
  closedCheckM openState shortname cmap openSeeShallow
displayPossibleContained shortname (ObjectsOn cmap) = do
  res <- concatMap (Data.List.NonEmpty.toList . snd)
          <$> filterPerceptiblesM cmap
  updateEnvironmentM ("You see the following on the " <> shortname)
  mapM_ updateEnvironmentM =<< mapM getShortNameM res
  -- updateEnvironmentM ("On the " <> shortname <> "you see:")
  -- mapM updateEnvironment 

caseContainmentAt :: Containment -> PossibleContained
caseContainmentAt (Containment container) = case container of
  (This (ContainedIn (ContainerInterface{..}) inv)) -> ObjectsIn (_openState',inv)
  (That (ContainedOn cmap)) -> ObjectsOn cmap
  (These cin cmap) -> AllObjects' (makeAllObjects cin cmap)

caseContainmentOn :: Containment -> Maybe (ContainerMap Object)
caseContainmentOn (Containment containment) = do
  case containment of
    (That (ContainedOn con)) -> Just con
    (These _ (ContainedOn con)) -> Just con
    _                        -> Nothing

makeAllObjects :: ContainedIn -> ContainedOn -> AllObjects
makeAllObjects (ContainedIn (ContainerInterface{..}) cIn') (ContainedOn cm) =
  AllObjects {
      _objectsIn' = (_openState',cIn')
    , _objectsOn' = cm
  }

caseContainmentIn :: Containment -> Maybe (OpenState, ContainerMap Object)
caseContainmentIn (Containment container) = case container of
  (This (ContainedIn (ContainerInterface{..}) inv)) -> Just (_openState',inv)
  (That _) -> Nothing
  (These (ContainedIn (ContainerInterface{..}) cIn') _) -> pure (_openState',cIn')
-- newtype ContainerMap a = ContainerMap 
--   { _unContainerMap' :: Map (Label a) (GIDList a)} 

filterPerceptiblesM :: ContainerMap Object -> GameStateExceptT [(Label Object, GIDList Object)]
filterPerceptiblesM (ContainerMap cin) = do
  res <- catMaybes <$> mapM capturePerceptiblesM (Data.Map.Strict.elems cin)
  let res' = concatMap (map fst . Data.List.NonEmpty.toList) res
  pure $ mapMaybe (capturePerceptibles res') (Data.Map.Strict.toList cin)
  where
  capturePerceptibles :: [GID Object]
                            -> (Label Object, GIDList Object)
                            -> Maybe (Label Object, GIDList Object)
  capturePerceptibles xs (label, neXS) =
      case mapMaybe (`packagePerceptible` xs) (Data.List.NonEmpty.toList neXS) of
        [] -> Nothing
        xs' -> Just (label, Data.List.NonEmpty.fromList xs')
      where
        packagePerceptible :: GID Object -> [GID Object] -> Maybe (GID Object)
        packagePerceptible gid = Data.List.find (gid ==)

closedCheckM :: OpenState
                  -> Text
                  -> ContainerMap Object
                  -> ( Maybe (NonEmpty (Label Object, NonEmpty Text)) -> Text)
                  -> GameStateExceptT ()
closedCheckM Open shortname cin openSee = do
  res <- filterPerceptiblesM cin
  if null res
    then updateEnvironmentM ("The " <> shortname <> "is empty.") -- ToDo
    else do
            dList <- makeDescriptionListM
                      $ ContainerMap (Data.Map.Strict.fromList res)
            let maybeSee = openSee dList
            updateEnvironmentM ("You see the following inside the " <> shortname)
            updateEnvironmentM maybeSee

closedCheckM Closed shortname _ _=
  updateEnvironmentM ("You can't look inside the "
                      <> shortname <> ", because it's closed.")
