module HauntedHouse.Game.Engine.VerbPhraseTwoEvaluator.Look where
import HauntedHouse.Recognizer.WordClasses
        (PrepPhrase (..), NounPhrase (..))
import HauntedHouse.Game.Model (GameStateExceptT)
import HauntedHouse.Tokenizer (Lexeme (..))
import HauntedHouse.Game.Model.World (Location(..), Object (..), Objects)
import Control.Monad.Except (throwError)
import HauntedHouse.Game.Location (getLocationM, getLocationIdM)
import HauntedHouse.Game.Model.Mapping
    ( LabelToGIDListMapping(_unLabelToGIDListMapping'), Label (..) )
import qualified Data.Map.Strict (lookup)
import HauntedHouse.Internal ( throwMaybeM, isVisible )
import Relude.Extra (fmapToFst)
import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Build.DescriptiveTemplate (visibleLabel)
import HauntedHouse.Game.Object (getObjectM)
import HauntedHouse.Clarifier (clarifyNotThere, clarifyWhich)
import HauntedHouse.Game.Model.Display (Display(..))
import qualified Data.List.NonEmpty

doLookObjectM :: PrepPhrase -> GameStateExceptT ()
doLookObjectM (PrepPhrase AT np ) = evaluateATNounPhrase np
doLookObjectM (PrepPhrase IN _)  = pass
doLookObjectM (PrepPhrase p _)   =
  throwError (show p <> " not evaulated in doLookObjectM" :: Text)
doLookObjectM (Preposition p) = throwError ("simple preposition "
                                  <> show p <> "not evaluated in doLookObjectM")

evaluateATNounPhrase :: NounPhrase -> GameStateExceptT ()
evaluateATNounPhrase (Noun noun) = do
  labelMap <- _unLabelToGIDListMapping' . _objectLabelMap'
                <$> (getLocationM =<< getLocationIdM)
  objectList <- Data.List.NonEmpty.toList
                <$> throwMaybeM maybeErr 
                    (Data.Map.Strict.lookup (Label noun) labelMap)

  visibleObjects <- catMaybes <$> mapM visibility objectList
  when (null visibleObjects) clarifyNotThere
  let visibleObjects' = Data.List.NonEmpty.fromList visibleObjects
  if length visibleObjects' == 1
    then display (head visibleObjects')
    else clarifyWhich visibleObjects'
  where
    maybeErr = "You don't see that here."

evaluateATNounPhrase _ = throwError "evaluateATNounPhrase incomplete"

visibility :: GID Object -> GameStateExceptT (Maybe (GID Object))
visibility gid = do
    conditions <- _conditions' <$> getObjectM gid
    pure $ if visibleLabel `elem` conditions then Just gid else Nothing