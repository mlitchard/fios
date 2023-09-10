{-# OPTIONS_GHC -Wno-type-defaults #-}
module HauntedHouse.Game.Engine.VerbPhraseThree.Look where
import HauntedHouse.Recognizer.WordClasses (PrepPhrase(PrepPhrase1), Noun, NounPhrase (..), Preposition)
import HauntedHouse.Tokenizer (Lexeme(..))
import HauntedHouse.Game.Model.World
import Control.Monad.Except (throwError)
import Data.Either.Combinators (whenRight, whenLeft, unlessLeft)
import HauntedHouse.Clarifier (findNoun, findAnchoredTo, findInDirectObject, clarifyingLookObjectM, subObjectAgreement, checkProximity, clarifyingLookSubjectM)
import HauntedHouse.Game.Model.Mapping (Label(..))
import HauntedHouse.Game.Object (getObjectGIDPairM, getObjectsFromLabelM, setObjectMapM, getObjectM)
import qualified Data.List.NonEmpty
import HauntedHouse.Game.Model.Display (describeObjectM, updateDisplayActionM, showPlayerActionM, showEnvironmentM, openSeeShallow, openSeeDeep, maybeDescribeNexusM)
import Data.These
import HauntedHouse.Game.Model.GID

doLookTwoPrepM :: (PrepPhrase, PrepPhrase) -> GameStateExceptT ()
doLookTwoPrepM (PrepPhrase1 prep np,pp) = do
  gsub@(gid,_) <- verifyExistenceNPPP np pp
  updateContainerDescriptionM prep gsub
  updatedSubject <- getObjectM gid
  maybeDescribeNexusM (_mNexus' updatedSubject)
  updateDisplayActionM (showPlayerActionM >> showEnvironmentM)
doLookTwoPrepM _ = throwError "doLookTwoPrep implementation unfinished"

-- assumes entity is container
-- throws error if it isn't
-- and then maintainer needs to sort the bad logic that got this far
updateContainerDescriptionM :: Preposition
                                -> (GID Object,Object)
                                -> GameStateExceptT ()
updateContainerDescriptionM prep (gid,entity) = do
  (Nexus nexus) <- throwMaybeM notContainerMSG (_mNexus' entity)
  Data.Either.Combinators.whenRight nexus (\_ -> throwError notContainerMSG)
  Data.Either.Combinators.whenLeft nexus (\(Containment container) ->
    do
      case container of
        (This containedIn) -> cIn containedIn >>= updateNexusM
        (That _) -> cOn
        (These _ _) -> cInOn
    )
  pass
  where
    updateNexusM :: Nexus -> GameStateExceptT ()
    updateNexusM nexus = do
      setObjectMapM gid (entity {_mNexus' = Just nexus})
    notContainerMSG = _shortName' entity <> "isn't a container. Fix your shit"
    cOn = throwError "cOn not implemented"
    cInOn = throwError "cInOn not implemented"
    cIn :: ContainedIn -> GameStateExceptT Nexus
    cIn  (ContainedIn (ContainerInterface' containerInterface) cmap) = do
      description <- case prep of
                AT -> pure $ openSeeShallow cmap
                IN -> pure $ openSeeDeep cmap
                _  -> throwError nonsenseMSG
      let updatedCInterface = ContainerInterface'
                                $ containerInterface{_describe' = description}
          container = Containment (This (ContainedIn updatedCInterface cmap))
      pure $ Nexus (Left container)
      where
        nonsenseMSG :: Text
        nonsenseMSG = "Nonsense Detected: updateContainerDescriptionM "
                        <> show prep
    cIn (ContainedIn PortalInterface _) = throwError nonsenseInterfaceMSG
      where
        nonsenseInterfaceMSG = "Containers that are portals aren't handled yet"
-- (ExceptT Text (StateT GameState IO)) 
chooseLookFunction :: Preposition -> GameStateExceptT (Object -> GameStateExceptT ())
chooseLookFunction AT = pure describeObjectM
chooseLookFunction IN = pure describeObjectM
chooseLookFunction prep = throwError ("Preposition unhandled :" <> show prep)

{-
data NounPhrase
  = NounPhrase1 Determiner NounPhrase
  | NounPhrase3 Number NounPhrase
  | Noun Noun
  deriving stock (Show, Eq, Ord)
-}
-- FIXME . change data Object to data Entity everywhere

verifyExistenceNPPP :: NounPhrase
                        -> PrepPhrase
                        -> GameStateExceptT (GID Object, Object)
verifyExistenceNPPP np pp = do
  possibleSubjects <- getObjectsFromLabelM subjectLabel
  anchoredEntities <- throwMaybeM (makeErrorReport np pp)
                      $ nonEmpty
                      $ filter (checkProximity pp)
                      $ mapMaybe findAnchoredTo
                      $ toList possibleSubjects
  possibleObjects@(object:|_) <- getObjectsFromLabelM objectLabel

  when (length possibleObjects > 1) $ do
    clarifyWhich <- _clarifyWhich' <$> ask
    clarifyWhich clarifyingLookObjectM (objectLabel, possibleObjects)
  -- toList anchoredEntities
  allMatches@(matched:|_) <- throwMaybeM (makeErrorReport np pp)
                  $ nonEmpty
                  $ mapMaybe (subObjectAgreement (fst object))
                  $ Data.List.NonEmpty.toList anchoredEntities
  when (length allMatches > 1) $ do
    let pairMatches = _anchoredObject' <$> allMatches
    clarifyWhich <- _clarifyWhich' <$> ask
    clarifyWhich clarifyingLookSubjectM (subjectLabel, pairMatches)
  pure (_anchoredObject' matched)
  where
    subjectLabel = Label $ findNoun np
    objectLabel  = Label $ findInDirectObject pp

makeErrorReport :: NounPhrase -> PrepPhrase -> Text
makeErrorReport _np _pp = "You don't see that here"