module HauntedHouse.Game.Engine.VerbPhraseTwoEvaluator.Look where
import HauntedHouse.Recognizer.WordClasses
        (PrepPhrase (..), NounPhrase (..))
import HauntedHouse.Game.Model.World
    ( GameStateExceptT, Location(..), Object(..), Objects, getLocationM, getLocationIdM, throwMaybeM )
import HauntedHouse.Tokenizer (Lexeme (..))
import Control.Monad.Except (throwError)
import HauntedHouse.Game.Model.Mapping
    ( LabelToGIDListMapping(..), Label (..) )
import qualified Data.Map.Strict (lookup)
import Relude.Extra (fmapToFst)
import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Build.DescriptiveTemplate (visibleLabel)
import HauntedHouse.Game.Object (getObjectM)
import HauntedHouse.Clarifier (clarifyNotThere, clarifyWhich)
import qualified Data.List.NonEmpty
import HauntedHouse.Game.Model.Display (describeObjectM, showPlayerActionM, showEnvironmentM)

doLookObjectM :: PrepPhrase -> GameStateExceptT ()
doLookObjectM (PrepPhrase AT np ) = evaluateATNounPhrase np
doLookObjectM (PrepPhrase IN _)  = pass
doLookObjectM (PrepPhrase p _)   =
  throwError (show p <> " not evaulated in doLookObjectM" :: Text)
doLookObjectM (Preposition p) = throwError ("simple preposition "
                                  <> show p <> "not evaluated in doLookObjectM")

-- _objectLabelMap'  :: LabelToGIDListMapping Object Object

evaluateATNounPhrase :: NounPhrase -> GameStateExceptT ()
evaluateATNounPhrase (Noun noun) = do
  (LabelToGIDListMapping m) <- _objectLabelMap'
                                <$> (getLocationM =<< getLocationIdM)
  objects <- throwMaybeM nopeErr $ Data.Map.Strict.lookup (Label noun) m
  if Data.List.NonEmpty.length objects == 1
    then describeObjectM (head objects) >> displayActionM
    else throwError "Multiple look object unimplemented"
  where
    displayActionM = showPlayerActionM >> showEnvironmentM
    nopeErr = "You don't see a " <> toText noun <> " here."


evaluateATNounPhrase _ = throwError "evaluate not completed"