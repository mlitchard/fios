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
        (verifySimple, identifyPossibleDirectObjects, PossibleDirectObjects (..))
import Game.Engine.Utilities (descriptiveLabel, directObjectLabel)
import Control.Monad.Except (MonadError(..))
import Game.Actions.Look.StandardLook (look)
import Game.Model.GID (GID)
import Game.Object (getObjectM)
import Data.Text (toLower)
import Build.ObjectTemplate (kitchenFloorGID)


-- (ImperativeClause (VerbPhrase2 LOOK (PrepPhrase2 IN THE (Adjective POT) (Noun PLANT))))
-- (ImperativeClause (VerbPhrase2 LOOK (PrepPhrase2 AT THE (Adjective POT) (Noun PLANT))))
evalLookObjectM :: PrepPhrase -> GameStateExceptT ()
evalLookObjectM (PrepPhrase1 prep np) = do
  pdo <- identifyPossibleDirectObjects np
  case pdo of
    NotFound x -> throwError ("You don't see a " <> toLower (show x) <> " here")
    Found gid -> do 
                  object <- getObjectM gid
                  doLookObject prep object
    Possibles gids -> throwError "possibles not completed"
 -- evaluateNounPhrase (clarifyingLookDirectObjectM prep) np 

{- evalLookObjectM (PrepPhrase2 prep _ (Adjective adj) (Noun noun)) =  do
  (_, entity) <- verifySimple descriptiveLabel' directObjectLabel'
  print ("doLookObjectM" <> _shortName' entity :: Text)

  let res = case prep of
              AT -> Right (lookAt entity)
              IN -> Right (lookIn entity)
              ON -> Right (lookOn entity)
              THROUGH -> Left lookThroughMsg
              _ -> Left nonsenseMsg
  _ <- error ("DEBUG" :: Text)
  either throwError (look entity) res

  updateDisplayActionM (showPlayerActionM >> showEnvironmentM)
  
  where
    lookAt =  _unLookAt' . _lookAt' . _lookFunctions' . _lookAction' . _standardActions'
    lookIn = _unLookIn' . _lookIn' . _lookFunctions' . _lookAction' . _standardActions'
    lookOn = _unLookOn' . _lookOn' . _lookFunctions' . _lookAction' . _standardActions'
    lookThroughMsg = "Look through not implemented" :: Text
    nonsenseMsg = "Arble and garble, I say to you." :: Text
    directObjectLabel' = directObjectLabel noun
    descriptiveLabel' = descriptiveLabel adj
-}
-- doLookObjectM pp@(PrepPhrase1 prep (Noun noun)) = pass
-- doLookObjectM pp@(PrepPhrase1 prep (NounPhrase1 _ (Noun noun))) = pass
-- doLookObjectM pp@(PrepPhrase1 prep (NounPhrase2 adj (Noun noun))) = pass
evalLookObjectM _  = throwError ("doLookObjectM not finished" :: Text) {- do
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