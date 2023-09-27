module Game.Engine.VerbPhraseTwoEvaluator.Look where
import Recognizer.WordClasses
        (PrepPhrase (..), NounPhrase (..), Preposition)
import Game.Model.Mapping
    ( LabelToGIDListMapping(..), Label (..) )
import qualified Data.Map.Strict (lookup)
import Game.Model.World
import qualified Data.List.NonEmpty
import Game.Model.Display
        (describeObjectM, showPlayerActionM, showEnvironmentM
        , updateDisplayActionM)
import qualified Relude.List.NonEmpty as NonEmpty
import Recognizer (Imperative, AdjPhrase (..))
import Clarifier (clarifyingLookDirectObjectM)
import Tokenizer.Data (Lexeme(..))
import Game.Engine.Verification (verifyAccessabilityPP, verifyAccessabilityAPNP, verifySimple)
import Game.Engine.Utilities (descriptiveLabel, directObjectLabel)
import Game.Actions.Look.StandardLook (look)
import Control.Monad.Except (MonadError(..))

{-
(ImperativeClause (VerbPhrase2 LOOK (PrepPhrase2 IN THE (Adjective POT) (Noun PLANT))))
(ImperativeClause (VerbPhrase2 LOOK (PrepPhrase2 AT THE (Adjective POT) (Noun PLANT))))
doLookObjectM :: PrepPhrase -> GameStateExceptT ()
doLookObjectM (PrepPhrase1 prep np) = 
  evaluateNounPhrase (clarifyingLookDirectObjectM prep) np 
doLookObjectM _                     = throwError "doLookObject implementation incomplete"
-}
doLookObjectM :: PrepPhrase -> GameStateExceptT ()
doLookObjectM (PrepPhrase2 prep _ (Adjective adj) (Noun noun)) = do
  (_, entity) <- verifySimple descriptiveLabel' directObjectLabel'
  print ("doLookObjectM" <> _shortName' entity :: Text)
  
  let res = case prep of
              AT -> Right (lookAt entity)
              IN -> Right (lookIn entity)
              ON -> Right (lookOn entity)
              THROUGH -> Left lookThroughMsg
              _ -> Left nonsenseMsg
  _ <- error ("DEBUG" :: Text)
 -- either throwError (look entity) res
  
  updateDisplayActionM (showPlayerActionM >> showEnvironmentM)
  where
    lookAt = _unLookAt' . _lookAt' . _lookAction' . _standardActions'
    lookIn = _unLookIn' . _lookIn' . _lookAction' . _standardActions'
    lookOn = _unLookOn' . _lookOn' . _lookAction' . _standardActions'
    lookThroughMsg = "Look through not implemented" :: Text
    nonsenseMsg = "Arble and garble, I say to you." :: Text
    directObjectLabel' = directObjectLabel noun
    descriptiveLabel' = descriptiveLabel adj

doLookObjectM pp@(PrepPhrase1 prep (Noun noun)) = pass
doLookObjectM pp@(PrepPhrase1 prep (NounPhrase1 _ (Noun noun))) = pass
doLookObjectM pp@(PrepPhrase1 prep (NounPhrase2 adj (Noun noun))) = pass
doLookObjectM _  = throwError ("doLookObjectM not finished" :: Text) {- do
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