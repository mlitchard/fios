module Game.Engine.Verification where
import Recognizer.WordClasses
    ( Imperative,
      NounPhrase(Noun, NounPhrase1, NounPhrase2),
      PrepPhrase(..) )
import Game.Model.GID (GID)
import qualified Data.List.NonEmpty
import Game.Object (getObjectGIDPairM)
import Game.Model.World as FoundObject (FoundObject (..))
import Game.Model.World
import Game.Model.Mapping (Label(..), LabelToGIDListMapping (..), GIDList)
import Control.Monad.Except (MonadError(..))
import Data.Map.Strict (lookup)
import Tokenizer (Lexeme (..))
import Game.Scene (tryDisplayF)
import Game.Model.Condition (Proximity (..))
import Recognizer (Adjective)


-- proximityMatch :: FoundObject 
matchAnchored :: FoundObject
                  -> FoundObject              --- adv       adj
                  -> GameStateExceptT (Maybe (FoundObject,FoundObject,Proximity))
matchAnchored advObject adjObject = do
  res <- tryAnchoredTo advOrientation
  pure $ case res of
    Nothing -> Nothing
    Just (AnchoredTo anchoredGid proximity)
      | anchoredGid == adjGid -> Just (advObject,adjObject,proximity)
      | otherwise -> Nothing
  where
    advOrientation = _orientation' (FoundObject._entity' advObject)
    adjGid =  FoundObject._gid' adjObject

tryAnchoredTo :: Orientation -> GameStateExceptT (Maybe AnchoredTo)
tryAnchoredTo (AnchoredTo' f) = Just <$> f
tryAnchoredTo _ = pure Nothing

evaluatePossibleObjects :: NonEmpty FoundObject
                            -> Maybe (NonEmpty FoundObject)
evaluatePossibleObjects possibles =
  let entities = evaluatePossibleObject <$> possibles
  in nonEmpty $ catMaybes $ toList entities

evaluatePossibleObject :: FoundObject -> Maybe FoundObject
evaluatePossibleObject (FoundObject gid entity) =
      let res = tryDisplayF entity
      in FoundObject gid <$> res

identifyPossiblelObjects :: NounPhrase
                                  -> GameStateExceptT (Label Object, Maybe PossibleObjects)

identifyPossiblelObjects (NounPhrase1 _ (Noun noun)) = do
  (LabelToGIDListMapping m) <- _objectLabelMap'
                                <$> (getLocationM =<< getLocationIdM)
  case Data.Map.Strict.lookup (Label noun) m of
    Nothing -> pure (Label noun,Nothing)
    Just (gid:|[]) -> do
                      objPair <- getObjectGIDPairM gid
                      pure (Label noun, Just (Found (makeFoundObject objPair)))
    Just gids -> do
                  objPairs <- mapM getObjectGIDPairM gids
                  let foundObjects = fmap makeFoundObject objPairs
                  pure (Label noun,Just (Possibles foundObjects))

identifyPossiblelObjects (NounPhrase2 adj (Noun noun)) = do
  (LabelToGIDListMapping m) <- _objectLabelMap'
                                <$> (getLocationM =<< getLocationIdM)
  case Data.Map.Strict.lookup (Label noun) m of
        Nothing -> pure (Label noun,Nothing)
        Just (gid:|[]) -> do
                            objPair <- getObjectGIDPairM gid
                            let found = makeFoundObject objPair
                            pure (Label noun,Just (Found found))
        Just gids -> do
                      objPairs <- mapM getObjectGIDPairM gids
                      let foundObjects = fmap makeFoundObject objPairs
                      pure (Label noun,Just (Possibles foundObjects))

identifyPossiblelObjects (Noun noun) = do
    (LabelToGIDListMapping m) <- _objectLabelMap'
                                  <$> (getLocationM =<< getLocationIdM)
    case Data.Map.Strict.lookup (Label noun) m of
      Nothing -> pure (Label noun, Nothing)
      Just (gid:|[]) -> do
                          objPair <- getObjectGIDPairM gid
                          let found = makeFoundObject objPair
                          pure (Label noun, Just (Found found))
      Just gids -> do 
                    objPairs <- mapM getObjectGIDPairM gids
                    let foundObjects = fmap makeFoundObject objPairs
                    pure (Label noun,Just (Possibles foundObjects))

identifyPossiblelObjects _ = throwError "identifyPossiblelObjects unfinished"

matchDescriptor :: Label Adjective
                    -> GIDList Object
                    -> GameStateExceptT (Maybe PossibleObjects)
matchDescriptor adj entityGids = do
  entities <- mapM getObjectGIDPairM entityGids
  let matched = nonEmpty $ Data.List.NonEmpty.filter
                (\(_,Object {..}) -> adj `elem` _descriptives') entities
  pure $ case matched of
                Nothing        -> Nothing
                Just (x :| []) -> Just (Found $ makeFoundObject x)
                Just xs        -> Just (Possibles $ makeFoundObject <$> xs)

makeFoundObject :: (GID Object, Object) -> FoundObject
makeFoundObject (gid, entity) = FoundObject {
    _gid' = gid
  , _entity' = entity
}

checkAdvObjProximity :: Lexeme -> Proximity -> Bool
checkAdvObjProximity lex proximity =
  case toProximity lex of
    Nothing -> False
    Just proximity' -> proximity == proximity'

toProximity :: Lexeme -> Maybe Proximity
toProximity ON = Just PlacedOn
toProximity UNDER = Just PlacedUnder
toProximity ABOVE = Just PlacedAbove
toProximity LEFT = Just PlacedLeft
toProximity RIGHT = Just PlacedRight
toProximity FRONT = Just PlacedFront
toProximity BEHIND = Just PlacedBehind
toProximity _ = Nothing

matchesProximity :: Proximity -> Lexeme -> Bool
matchesProximity PlacedOn ON = True
matchesProximity PlacedUnder UNDER = True
matchesProximity PlacedAbove ABOVE = True
matchesProximity PlacedLeft LEFT = True
matchesProximity PlacedRight RIGHT = True
matchesProximity PlacedFront FRONT = True
matchesProximity PlacedBehind BEHIND = True
matchesProximity _ _ = False

visibilityExistence :: NounPhrase
                -> GameStateExceptT (Label Object,NonEmpty FoundObject)
visibilityExistence np = do
  padvo <- identifyPossiblelObjects np
  case padvo of
    (label, Nothing) -> throwError (objNotFound label)
    (label,Just (Found obj')) -> do
                                  obj <- throwMaybeM (objNotFound label)
                                          $ evaluatePossibleObject obj'
                                  pure $ (,) label (Data.List.NonEmpty.singleton obj)
    (label, Just (Possibles objs')) -> do
                                        objs <- throwMaybeM (objNotFound label)
                                                  $ evaluatePossibleObjects objs'
                                        pure (label,objs)

  where
    objNotFound (Label label) =
      "You don't see a " <> show label <> " here."

type ResultAccessabilityNP =
  (Either (Label Object, NonEmpty GIDObjectPair) GIDObjectPair)

type ResultAccessabilityPP =
      GameStateExceptT (Either (GameStateExceptT ()) (GID Object, Object))
