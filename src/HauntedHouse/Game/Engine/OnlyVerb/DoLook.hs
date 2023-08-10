module HauntedHouse.Game.Engine.OnlyVerb.DoLook where

import HauntedHouse.Game.Model (GameStateExceptT)

doLookM :: GameStateExceptT ()
doLookM = do
  print ("entered doLook" :: String)
{-
  location' <- getLocationM =<< getLocationIdM
  printTitleM (_title' location') 
  printDescriptionM (_description' location')
  printObjectsM (_objects' location')
-}
  pass
  
printTitleM :: Text -> GameStateExceptT ()
printTitleM title = print (("You're in the " :: String) <> show title) 

printDescriptionM :: Text -> GameStateExceptT () 
printDescriptionM _ = pass 

-- newtype Objects
--  = Objects {_unObjects' :: Data.List.NonEmpty.NonEmpty (GID Object)}
--      deriving stock Show

-- getObjectFromMapM :: GID Object -> GameStateExceptT Object 

{-
printSceneM :: Maybe Objects -> GameStateExceptT () 
printSceneM Nothing = print ("This room is empty" :: String)
printSceneM (Just (Objects oMap')) = 
  mapM_ (printObjectM <=< getObjectFromMapM) oMap'
{-

data Object = Object
  { _related'       :: Relations
  , _moveability'   :: Moveablility
  , _odescription'  :: Text
  , _isContainer'   :: Maybe Container
  } deriving stock Show

-}

printObjectM :: Object -> GameStateExceptT () 
printObjectM (Object related _ description isContainer)= do 
  print description
  mapM_ print anchored
  where 
    relationships :: [Text]
    relationships  

-}

