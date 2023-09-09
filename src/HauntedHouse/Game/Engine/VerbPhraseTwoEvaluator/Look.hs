module HauntedHouse.Game.Engine.VerbPhraseTwoEvaluator.Look where
import HauntedHouse.Recognizer.WordClasses
        (PrepPhrase (..), NounPhrase (..))
import HauntedHouse.Tokenizer (Lexeme (..))
import Control.Monad.Except (throwError, MonadError (..))
import HauntedHouse.Game.Model.Mapping
    ( LabelToGIDListMapping(..), Label (..) )
import qualified Data.Map.Strict (lookup)
import HauntedHouse.Game.Object (capturePerceptibleM)
import HauntedHouse.Game.Model.World 
import qualified Data.List.NonEmpty
import HauntedHouse.Game.Model.Display 
        (describeObjectM, showPlayerActionM, showEnvironmentM
        , updateDisplayActionM)
import qualified Relude.List.NonEmpty as NonEmpty
import HauntedHouse.Clarifier (clarifyingLookSubjectM)

doLookObjectM :: PrepPhrase -> GameStateExceptT ()
doLookObjectM (PrepPhrase1 AT np) = evaluateATNounPhrase np 
doLookObjectM _                   = throwError "doLookObject implementation incomplete"
{-
doLookObjectM (PrepPhrase1 AT np ) = evaluateATNounPhrase np `catchError` errorSee
doLookObjectM (PrepPhrase1 IN _)  = pass
doLookObjectM (PrepPhrase1 p _)   =
  throwError (show p <> " not evaulated in doLookObjectM" :: Text)
doLookObjectM (Preposition p) = throwError ("simple preposition "
                                  <> show p <> "not evaluated in doLookObjectM")
-}
errorSee :: Text -> GameStateExceptT ()
errorSee = print 

-- _objectLabelMap'  :: LabelToGIDListMapping Object Object

evaluateATNounPhrase :: NounPhrase -> GameStateExceptT ()
evaluateATNounPhrase (Noun noun) = do
  (LabelToGIDListMapping m) <- _objectLabelMap'
                                <$> (getLocationM =<< getLocationIdM)
  objects <- throwMaybeM nopeErr 
              =<< capturePerceptibleM 
              =<< throwMaybeM nopeErr (Data.Map.Strict.lookup (Label noun) m)
  if Data.List.NonEmpty.length objects == 1
    then describeObjectM (snd . NonEmpty.head $ objects) 
          >> updateDisplayActionM displayActionM
    else do
          clarifyWhich <- _clarifyWhich' <$> ask 
          clarifyWhich clarifyingLookSubjectM (Label noun, objects)
  where
    displayActionM = showPlayerActionM >> showEnvironmentM
    nopeErr = "You don't see a " <> toText noun <> " here."


evaluateATNounPhrase _ = throwError "evaluate not completed"