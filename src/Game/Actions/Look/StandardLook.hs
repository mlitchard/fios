module Game.Actions.Look.StandardLook where
import Game.Model.World
import Game.Model.GID
import Game.Model.Mapping
import qualified Data.Map.Strict
import Game.Model.Display (updateEnvironmentM, updatePlayerActionM)
import Game.Object (getShortNameM)
import qualified Data.List.NonEmpty
import qualified Data.Text
  
look :: Object
          -> LookF
          -> GameStateExceptT ()
look entity lookFunction = do
  worldCMap <- _unGIDToDataMapping' . _containerMap' . _world' <$> get
  lookFunction entity worldCMap

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
lookAtOpenBoxM gid (Object {..}) cmap = 
 {- updatePlayerActionM msg >> -} lookContainerM gid shallowLookInContainerM cmap

lookInOpenBoxM :: GID Object
                    -> Object
                    -> (Map (GID Object) Container -> GameStateExceptT ())
lookInOpenBoxM gid (Object {..}) cmap = 
  {- updatePlayerActionM msg >> -} lookContainerM gid deepLookInContainerM cmap


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
shallowLookInContainerM container
  | Data.Map.Strict.null container = updateEnvironmentM emptyMsg
  | otherwise                      = updateEnvironmentM somethingMsg
  where
    emptyMsg = "it's empty."
    somethingMsg = "You see something inside."

deepLookInContainerM :: Map (Label Object) (GIDList Object)
                          -> GameStateExceptT ()
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
