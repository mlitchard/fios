module Game.Actions.Look.StandardLook where
import Game.Model.World
import Game.Model.GID
import Game.Model.Mapping
import qualified Data.Map.Strict
import Game.Model.Display (updateEnvironmentM, updatePlayerActionM)
import Game.Object (getShortNameM)
import qualified Data.Text

look :: Object
          -> LookF
          -> GameStateExceptT ()
look entity lookFunction = do
  worldCMap <- _unGIDToDataMap' . _containerMap' . _world' <$> get
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
{- updatePlayerActionM msg >> -}
lookAtOpenBoxM :: GID Object
                    -> Object
                    -> (Map (GID Object) Container -> GameStateExceptT ())
lookAtOpenBoxM gid (Object {..}) =
  lookContainerM msg gid shallowLookInContainerM
  where
    msg = "You look at the " <> _shortName'
{- updatePlayerActionM msg >> -}

-- ToDo merge lookAtShelf and lookInOpenBox
lookAtShelfM :: GID Object 
                  -> Object 
                  -> (Map (GID Object) Container -> GameStateExceptT ())
lookAtShelfM gid (Object{..}) = 
  lookContainerM msg gid (deepLookInContainerM onMsg) 
  where 
    msg = "You look at the " <> _shortName'
    onMsg = "On it you see:" 

lookInOpenBoxM :: GID Object
                    -> Object
                    -> (Map (GID Object) Container -> GameStateExceptT ())
lookInOpenBoxM gid (Object {..}) =
  lookContainerM msg gid (deepLookInContainerM insideMsg)
  where
    msg = "You look in the " <> _shortName'
    insideMsg = "Inside the " <> _shortName' <> "you see:"

lookAtClosedBoxM :: Object -> GameStateExceptT ()
lookAtClosedBoxM entity = do
  lookDescriptionM entity
  updateEnvironmentM "It's closed."

lookInClosedBoxM :: GameStateExceptT ()
lookInClosedBoxM = updateEnvironmentM msg
  where
    msg = "You can't look in that because it's closed"

lookContainerM :: Text
                  -> GID Object
                  -> (Map (Label Object) (NonEmpty ContainedEntity) -> GameStateExceptT ())
                  -> Map (GID Object) Container
                  -> GameStateExceptT ()
lookContainerM msg gid lookFunction worldCMap = do
  -- (_unContainerMap' . _unContainer')
  cMap <- _unContainer'
            <$> throwMaybeM errMsg (Data.Map.Strict.lookup gid worldCMap)
  if Data.Map.Strict.null cMap
    then updateEnvironmentM "It's empty"
    else updatePlayerActionM msg >> lookFunction cMap
  where
    errMsg = "lookInContainer is being used on an entity that is not a container"

shallowLookInContainerM :: Map (Label Object) (NonEmpty ContainedEntity)
                            -> GameStateExceptT ()
shallowLookInContainerM container
  | Data.Map.Strict.null container = updateEnvironmentM emptyMsg
  | otherwise                      = updateEnvironmentM somethingMsg
  where
    emptyMsg = "it's empty."
    somethingMsg = "You see something inside."

-- FixMe processing Map should be done by caller
-- Pass in a NonEmpty
deepLookInContainerM :: Text 
                          -> Map (Label Object) (NonEmpty ContainedEntity)
                          -> GameStateExceptT ()
deepLookInContainerM msg cMap = do
  shortNames <- mapM (getShortNameM . _containedGid')
                  $ concatMap toList $ Data.Map.Strict.elems cMap
  updateEnvironmentM msg
  mapM_ updateEnvironmentM shortNames

changeLookAction :: LookAction -> Object -> Object
changeLookAction lookAction' entity@(Object {..}) =
  let standardActions' = _standardActions'
  in entity{_standardActions' = standardActions' {_lookAction' = lookAction'}}
