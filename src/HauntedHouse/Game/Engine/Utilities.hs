module HauntedHouse.Game.Engine.Utilities where
import HauntedHouse.Game.Model.Condition (Proximity (..))
import HauntedHouse.Tokenizer (Lexeme (..))
import HauntedHouse.Recognizer
import HauntedHouse.Game.Model.World
import qualified Data.Map.Strict
import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Game.Model.Mapping
import qualified Data.List.NonEmpty
import Data.Map.Strict (delete)

maybeContainment :: Nexus -> Maybe Containment 
maybeContainment (Containment' containment) = Just containment 
maybeContainment _ = Nothing 

{-
data ContainedIn = ContainedIn
  { _containerInterface'  :: ContainerInterface
  , _containedIn'         :: ContainerMap Object
  }
-}

 --  case                           
removeFromContainedInM :: GID Object 
                          -> Label Object 
                          -> ContainedIn 
                          -> GameStateExceptT ContainedIn 
removeFromContainedInM gid label containedIn@(ContainedIn {..}) = do
  let cmap = _unContainerMap' _containedIn'  
  gidList <- throwMaybeM noExist $ Data.Map.Strict.lookup label cmap
  let updatedGIDList = nonEmpty $ Data.List.NonEmpty.filter (/= gid) gidList 
      updatedMap = ContainerMap $ case updatedGIDList of 
                    Nothing -> Data.Map.Strict.delete label cmap 
                    Just xs  -> Data.Map.Strict.insert label xs cmap 
  pure containedIn{_containedIn' = updatedMap}
  where
    noExist = "removeFromContainedIn: Doesn't exist " <> show label

removeFromShelfM :: GID Object 
                          -> Label Object 
                          -> Shelf
                          -> GameStateExceptT Shelf
removeFromShelfM gid label shelf@(Shelf {..}) = do 
  let cmap = _unContainerMap' _shelf'     
  gidList <- throwMaybeM noExist $ Data.Map.Strict.lookup label cmap
  let updatedGIDList = nonEmpty $ Data.List.NonEmpty.filter (/= gid) gidList
      updatedMap = ContainerMap $ case updatedGIDList of 
                    Nothing -> Data.Map.Strict.delete label cmap 
                    Just xs  -> Data.Map.Strict.insert label xs cmap 
  pure shelf{_shelf' = updatedMap}
  where
    noExist = "removeFromContainedIn: Doesn't exist " <> show label
toOnOrIn :: Orientation ->  Maybe OnOrIn 
toOnOrIn (ContainedBy' (ContainedBy containedBy _)) = Just containedBy 
toOnOrIn (Floor gid) = Just (On gid)
toOnOrIn _  = Nothing

prepToProximity :: Lexeme -> Maybe Proximity 
prepToProximity ON = Just PlacedOn
prepToProximity UNDER = Just PlacedUnder
prepToProximity ABOVE = Just PlacedAbove
prepToProximity LEFT = Just PlacedLeft 
prepToProximity RIGHT = Just PlacedRight
prepToProximity FRONT = Just PlacedFront 
prepToProximity BEHIND = Just PlacedBehind 
prepToProximity _ = Nothing   


{-
data PrepPhrase
  = PrepPhrase1 Preposition NounPhrase
  | PrepPhrase2 Preposition  Determiner AdjPhrase NounPhrase
  -- | Preposition Preposition
  deriving stock (Show, Eq, Ord)
-}
prepositionFromPhrase :: PrepPhrase -> Preposition 
prepositionFromPhrase (PrepPhrase1 prep _)      = prep 
prepositionFromPhrase (PrepPhrase2 prep _ _ _)  = prep

