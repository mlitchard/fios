module Game.Engine.VerbPhraseTwoEvaluator.Look where
import Recognizer.WordClasses
        (PrepPhrase (..), NounPhrase (..))
import Game.Model.World
import Game.Model.Display
        (showPlayerActionM, showEnvironmentM
        , updateDisplayActionM)
import Recognizer (AdjPhrase (..))
import Tokenizer.Data (Lexeme(..))
import Game.Engine.Verification
        (verifySimple, identifyPossibleDirectObjects, PossibleDirectObjects (..), evaluatePossibleDirectObject, evaluatePossibleDirectObjects)
import Game.Engine.Utilities (descriptiveLabel, directObjectLabel)
import Control.Monad.Except (MonadError(..))
import Game.Actions.Look.StandardLook (look)
import Game.Model.GID (GID)
import Game.Object (getObjectM)
import Data.Text (toLower)
import Build.ObjectTemplate (kitchenFloorGID)
import Clarifier (clarifyingLookDirectObjectM)


-- (ImperativeClause (VerbPhrase2 LOOK (PrepPhrase2 IN THE (Adjective POT) (Noun PLANT))))
-- (ImperativeClause (VerbPhrase2 LOOK (PrepPhrase2 AT THE (Adjective POT) (Noun PLANT))))
evalLookObjectM :: PrepPhrase -> GameStateExceptT ()
evalLookObjectM (PrepPhrase1 prep np) = do
  pdo <- identifyPossibleDirectObjects np
  case pdo of
    (label,NotFound) -> throwError ("You don't see that here")
    (label,Found gid) -> do 
                  entity <- throwMaybeM "You don't see that here" 
                              =<< evaluatePossibleDirectObject gid
                  doLookObject prep entity
    (label,Possibles gids) -> do 
                                entities <- throwMaybeM "You don't see that here"
                                              =<< evaluatePossibleDirectObjects gids
                                clarifyWhich <- _clarifyWhich' <$> ask 
                                clarifyWhich (clarifyingLookDirectObjectM prep) (label, entities)
                                pass

 -- evaluateNounPhrase (clarifyingLookDirectObjectM prep) np 

evalLookObjectM (PrepPhrase2 prep _ (Adjective adj) noun) =  do
  pdo <- identifyPossibleDirectObjects (NounPhrase2 adj noun)
  case pdo of 
    (label,NotFound) -> throwError ("You don't see that here")
    (label,Found gid) -> do 
                  entity <- throwMaybeM "You don't see that here" 
                              =<< evaluatePossibleDirectObject gid
                  doLookObject prep entity
    (label,Possibles gids) -> pass 
  

-- doLookObjectM pp@(PrepPhrase1 prep (Noun noun)) = pass
-- doLookObjectM pp@(PrepPhrase1 prep (NounPhrase1 _ (Noun noun))) = pass
-- doLookObjectM pp@(PrepPhrase1 prep (NounPhrase2 adj (Noun noun))) = pass
evalLookObjectM _  = throwError ("evalLookObjectM not finished" :: Text) {- do
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
doLookObject :: Lexeme -> Object -> GameStateExceptT ()
doLookObject prep entity@(Object {..})= do
  let lookf =  case prep of 
            AT -> lookFunctions._lookAt'._unLookAt'
            IN -> lookFunctions._lookIn'._unLookIn'
            ON -> lookFunctions._lookOn'._unLookOn'
            _ -> const (const (throwError lookAbsurd))
  look entity lookf  
  updateDisplayActionM (showPlayerActionM >> showEnvironmentM)
  pass
  where 
    lookAbsurd = "Think hard about what you just tried to do."
    lookFunctions = _standardActions'._lookAction'._lookFunctions'
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