module HauntedHouse.Game.Engine.VerbPhraseTwoEvaluator.Look where
import HauntedHouse.Recognizer.WordClasses
        (PrepPhrase (..), NounPhrase (..))
import HauntedHouse.Game.Model.World
    ( GameStateExceptT, Location(..), Object(..), Objects, getLocationM, getLocationIdM, throwMaybeM )
import HauntedHouse.Tokenizer (Lexeme (..))
import Control.Monad.Except (throwError, MonadError (..))
import HauntedHouse.Game.Model.Mapping
    ( LabelToGIDListMapping(..), Label (..) )
import qualified Data.Map.Strict (lookup)
import Relude.Extra (fmapToFst)
import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Build.DescriptiveTemplate (visibleLabel)
import HauntedHouse.Game.Object (getObjectM, capturePerceptibleM)
import HauntedHouse.Clarifier (clarifyNotThere, clarifyWhich)
import qualified Data.List.NonEmpty
import HauntedHouse.Game.Model.Display (describeObjectM, showPlayerActionM, showEnvironmentM, updateDisplayActionM)
import qualified Relude.List.NonEmpty as NonEmpty

doLookObjectM :: PrepPhrase -> GameStateExceptT ()
doLookObjectM (PrepPhrase AT np ) = evaluateATNounPhrase np `catchError` errorSee
doLookObjectM (PrepPhrase IN _)  = pass
doLookObjectM (PrepPhrase p _)   =
  throwError (show p <> " not evaulated in doLookObjectM" :: Text)
doLookObjectM (Preposition p) = throwError ("simple preposition "
                                  <> show p <> "not evaluated in doLookObjectM")

errorSee :: Text -> GameStateExceptT ()
errorSee = print 

-- _objectLabelMap'  :: LabelToGIDListMapping Object Object

evaluateATNounPhrase :: NounPhrase -> GameStateExceptT ()
evaluateATNounPhrase (Noun noun) = do
  print "evaluatedAtNounPhrase"
  (LabelToGIDListMapping m) <- _objectLabelMap'
                                <$> (getLocationM =<< getLocationIdM)
  print "got gid list"
  objects <- throwMaybeM nopeErr 
              =<< capturePerceptibleM 
              =<< throwMaybeM nopeErr (Data.Map.Strict.lookup (Label noun) m)
  print "got object list"
  if Data.List.NonEmpty.length objects == 1
    then describeObjectM (NonEmpty.head objects) 
          >> updateDisplayActionM displayActionM
    else clarifyWhich (Label noun, objects)
  where
    displayActionM = showPlayerActionM >> showEnvironmentM
    nopeErr = "You don't see a " <> toText noun <> " here."


evaluateATNounPhrase _ = throwError "evaluate not completed"