module Game.Engine.VerbPhraseTwoEvaluator.Look where
import Recognizer.WordClasses
        (PrepPhrase (..), NounPhrase (..), Preposition)
import Control.Monad.Except (throwError, MonadError (..))
import Game.Model.Mapping
    ( LabelToGIDListMapping(..), Label (..) )
import qualified Data.Map.Strict (lookup)
import Game.Model.World
import qualified Data.List.NonEmpty
import Game.Model.Display
        (describeObjectM, showPlayerActionM, showEnvironmentM
        , updateDisplayActionM)
import qualified Relude.List.NonEmpty as NonEmpty
import Recognizer (Imperative)
import Clarifier (clarifyingLookDirectObjectM)
import Tokenizer.Data (Lexeme(..))
import Game.Engine.Verification (verifyAccessabilityPP, verifyAccessabilityAPNP)

{-
(ImperativeClause (VerbPhrase2 LOOK (PrepPhrase2 IN THE (Adjective POT) (Noun PLANT))))
(ImperativeClause (VerbPhrase2 LOOK (PrepPhrase2 AT THE (Adjective POT) (Noun PLANT))))
doLookObjectM :: PrepPhrase -> GameStateExceptT ()
doLookObjectM (PrepPhrase1 prep np) = 
  evaluateNounPhrase (clarifyingLookDirectObjectM prep) np 
doLookObjectM _                     = throwError "doLookObject implementation incomplete"
-}
doLookObjectM :: PrepPhrase -> GameStateExceptT ()
doLookObjectM pp@(PrepPhrase1 prep _) = pass {- do
  res <- verifyAccessabilityPP (clarifyingLookDirectObjectM prep) pp
  case res of
    (Left clarifyM) -> clarifyM
    (Right (_, Object{..})) -> maybeDescribeNexusM _mNexus'

 -- evaluateNounPhrase (clarifyingLookDirectObjectM prep) np 
 -- (clarifyingLookDirectObjectM prep)
doLookObjectM (PrepPhrase2 prep _ ap np) = pass  do
  (_,entity@(Object {..})) <- verifyAccessabilityAPNP ap np
  case prep of 
    AT -> do
            print ("doing look at" :: Text) 
            _lookAt' _standardActions' entity
    IN -> _lookIn' _standardActions' entity
    ON -> _lookOn' _standardActions' entity
    _ -> throwError "Think hard about what you just tried to do."
  updateDisplayActionM (showPlayerActionM >> showEnvironmentM)
  -}
errorSee :: Text -> GameStateExceptT ()
errorSee = print
{-
describeObjectM (snd . NonEmpty.head $ objects) 
          >> updateDisplayActionM displayActionM
-}
-- FIXME seperate verification
{-
evaluateNounPhrase :: (Imperative -> GameStateExceptT ()) 
                        -> NounPhrase 
                        -> GameStateExceptT ()
evaluateNounPhrase clarifier (Noun noun) = do
  (LabelToGIDListMapping m) <- _objectLabelMap'
                                <$> (getLocationM =<< getLocationIdM)
  objects <- throwMaybeM nopeErr 
              =<< capturePerceptiblesM 
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
-}