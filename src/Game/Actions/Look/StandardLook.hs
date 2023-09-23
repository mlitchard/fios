module Game.Actions.Look.StandardLook where
import Game.Model.World
import Game.Model.GID
import Game.Model.Mapping
import qualified Data.Map.Strict
import Game.Model.Display (updateEnvironmentM, updatePlayerActionM)
import Game.Object (getShortNameM)
import qualified Data.List.NonEmpty
import qualified Data.Text
import Game.Model.Condition (Perceptibility(..))

makeLookAction :: (Object -> GameStateExceptT ())
                -> LookAtF 
                -> LookOnF
                -> LookInF
                -> LookAction
makeLookAction changeFunction lookAt lookOn lookIn = LookAction {
    _updateLook' = changeFunction 
  , _look' = look lookAt lookOn lookIn 
}

look :: LookAtF
          -> LookOnF
          -> LookInF
          -> LookPrep
          -> Object
          -> GameStateExceptT ()
look lookAtF lookOnF lookInF lookPrep entity@(Object {..}) = do
  worldCMap <- _unGIDToDataMapping' . _containerMap' . _world' <$> get
  let lookFunction = case _perceptability' of
                      Perceptible -> selectFunction
                      Imperceptible -> const (updateEnvironmentM noSee)
  lookFunction worldCMap
  where
    selectFunction = case lookPrep of
      LookAt -> _unLookAt' lookAtF entity
      LookOn -> _unLookOn' lookOnF entity
      LookIn -> _unLookIn' lookInF entity
      LookThrough -> const (updateEnvironmentM lookThroughError)
    noSee = "You don't see a " <> _shortName' <> " here."
    lookThroughError = "Haven't implemented look through yet,"
                        <> " but you probably can't do what you are trying to do."

lookDescriptionM :: Object -> GameStateExceptT ()
lookDescriptionM (Object {..}) =
  updatePlayerActionM ("You look at the " <> _shortName')
    >> updateEnvironmentM (Data.Text.concat _odescription')
-- updateEnvironmentM (Data.Text.concat _odescription') 
--                >> lookContainerM gid worldCMap shallowLookInContainerM

lookInImpossibleM :: GameStateExceptT ()
lookInImpossibleM = updateEnvironmentM msg
  where
    msg = "X-ray vision would be cool, but you don't have that"

lookAtOpenBoxM :: GID Object
                    -> Object
                    -> (Map (GID Object) Container -> GameStateExceptT ())
lookAtOpenBoxM gid (Object {..}) = \cmap ->
  updatePlayerActionM msg >> lookContainerM gid shallowLookInContainerM cmap
  where
    msg = "You look at the " <> _shortName'

lookInOpenBoxM :: GID Object
                    -> Object
                    -> (Map (GID Object) Container -> GameStateExceptT ())
lookInOpenBoxM gid (Object {..}) = \cmap ->
  updatePlayerActionM msg >> lookContainerM gid deepLookInContainerM cmap
  where
    msg = "You look in the " <> _shortName'

lookAtClosedBoxM :: Object -> GameStateExceptT ()
lookAtClosedBoxM entity = do
  lookDescriptionM entity
  updateEnvironmentM "It's closed."

lookInClosedBoxM :: GameStateExceptT ()
lookInClosedBoxM = updateEnvironmentM msg
  where
    msg = "You can't look in that because it's closed"

lookContainerM :: GID Object
                  -> (Map (Label Object) (GIDList Object) -> GameStateExceptT ())
                  -> Map (GID Object) Container
                  -> GameStateExceptT ()
lookContainerM gid lookFunction worldCMap = do
  -- (_unContainerMap' . _unContainer')
  (ContainerMap cMap) <- _unContainer'
            <$> throwMaybeM errMsg (Data.Map.Strict.lookup gid worldCMap)
  if Data.Map.Strict.null cMap
    then updateEnvironmentM "It's empty"
    else lookFunction cMap
  where
    errMsg = "lookInContainer is being used on an entity that is not a container"

shallowLookInContainerM :: Map (Label Object) (GIDList Object)
                            -> GameStateExceptT ()
shallowLookInContainerM _ = updateEnvironmentM "You see something inside."

deepLookInContainerM :: Map (Label Object) (GIDList Object) -> GameStateExceptT ()
deepLookInContainerM cMap = do
  shortNames <- mapM getShortNameM
                  $ concatMap Data.List.NonEmpty.toList
                  $ Data.Map.Strict.elems cMap
  updateEnvironmentM "Inside you see:"
  mapM_ updateEnvironmentM shortNames

changeLookAction :: LookAction -> Object -> Object 
changeLookAction lookAction' entity@(Object {..}) = 
  let standardActions' = _standardActions'
  in entity{_standardActions' = standardActions' {_lookAction' = lookAction'}}
