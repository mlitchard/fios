module HauntedHouse.Game.Engine.VerbPhraseTwoEvaluator.Look where
import HauntedHouse.Recognizer.WordClasses
        (PrepPhrase (..), NounPhrase (..), Preposition)
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
import HauntedHouse.Recognizer (Imperative)
import HauntedHouse.Clarifier (clarifyingLookDirectObjectM)
import HauntedHouse.Tokenizer.Data (Lexeme(..))
import HauntedHouse.Game.Engine.Verification (verifyAccessabilityPP)

{-
(ImperativeClause (VerbPhrase2 LOOK (PrepPhrase2 IN THE (Adjective POT) (Noun PLANT))))
(ImperativeClause (VerbPhrase2 LOOK (PrepPhrase2 AT THE (Adjective POT) (Noun PLANT))))
doLookObjectM :: PrepPhrase -> GameStateExceptT ()
doLookObjectM (PrepPhrase1 prep np) = 
  evaluateNounPhrase (clarifyingLookDirectObjectM prep) np 
doLookObjectM _                     = throwError "doLookObject implementation incomplete"
-}
doLookObjectM :: PrepPhrase -> GameStateExceptT ()
doLookObjectM pp@(PrepPhrase1 _ _) = do
  _ <- verifyAccessabilityPP pp
  pass 
 -- evaluateNounPhrase (clarifyingLookDirectObjectM prep) np 
 -- (clarifyingLookDirectObjectM prep)
doLookObjectM (PrepPhrase2 prep _ ap np) = throwError "doLookOject unfinished"


errorSee :: Text -> GameStateExceptT ()
errorSee = print 
{-
describeObjectM (snd . NonEmpty.head $ objects) 
          >> updateDisplayActionM displayActionM
-}
-- FIXME seperate verification
evaluateNounPhrase :: (Imperative -> GameStateExceptT ()) 
                        -> NounPhrase 
                        -> GameStateExceptT ()
evaluateNounPhrase clarifier (Noun noun) = do
  (LabelToGIDListMapping m) <- _objectLabelMap'
                                <$> (getLocationM =<< getLocationIdM)
  objects <- throwMaybeM nopeErr 
              =<< capturePerceptibleM 
              =<< throwMaybeM nopeErr (Data.Map.Strict.lookup (Label noun) m)
  if Data.List.NonEmpty.length objects == 1
    then describeObjectM (snd . NonEmpty.head $ objects) 
          >> updateDisplayActionM displayActionM
    else do
       --   mapM_ (updateContainerDescriptionM prep) objects
          clarifyWhich <- _clarifyWhich' <$> ask 
          clarifyWhich clarifier (Label noun, objects)
  where
    displayActionM = showPlayerActionM >> showEnvironmentM
    nopeErr = "You don't see a " <> toText noun <> " here."
evaluateNounPhrase _ _ = throwError "evaluateNounPhrase: evaluate not completed"