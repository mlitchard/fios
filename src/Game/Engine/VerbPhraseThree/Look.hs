{-# OPTIONS_GHC -Wno-type-defaults #-}
module Game.Engine.VerbPhraseThree.Look where
import Recognizer.WordClasses (PrepPhrase(PrepPhrase1))
import Game.Model.World
import Control.Monad.Except (throwError)
import Game.Engine.Verification
    ( identifyPossiblelObjects,
      matchObjects,
      matchesProximity, evaluatePossibleObject )
import Game.Model.Display (updateEnvironmentM)
import Game.Engine.Verbs.Look (whichLook, doLookObject)
-- (_lookIn' _standardActions') _shortName'
doLookTwoPrepM :: (PrepPhrase, PrepPhrase) -> GameStateExceptT ()
doLookTwoPrepM (PrepPhrase1 advPrep advNP,PrepPhrase1 adjPrep adjNP) = do
  padvo <- identifyPossiblelObjects advNP
  case padvo of
    (label,Nothing) -> updateEnvironmentM "You don't see that here" >> pass
    (label,Just (Found advObj')) -> do
                  advObj <- throwMaybeM "That makes no sense"
                              $ evaluatePossibleObject advObj'
                  padjo <- identifyPossiblelObjects adjNP
                  case padjo of
                    (_,Nothing) -> updateEnvironmentM "That makes no sense" >> pass
                    (label', Just (Found adjObj')) -> do
                                                adjObj <- throwMaybeM "That makes no sense"
                                                            $ evaluatePossibleObject adjObj'
                                                proximity <- throwMaybeM "That's not where you think it is"
                                                              =<< matchObjects advObj adjObj
                                                if matchesProximity proximity adjPrep
                                                  then doLookObject advPrep (_entity' advObj')
                                                  else updateEnvironmentM "That's not where you think it is."
                    (label', Just (Possibles advGids)) -> pass
    (label,Just (Possibles gids)) -> pass
  {- do
  (_,entity@(Object {..})) <- verifySensibilityNPPP clarifying np pp
  void $ case prep of
    IN -> _lookIn' _standardActions' entity
    ON -> _lookOn' _standardActions' entity
    AT -> _lookAt' _standardActions' entity
    _ -> throwError "Think hard about what you just tried to do."
  -- maybeDescribeNexusM _mNexus'
  updateDisplayActionM (showPlayerActionM >> showEnvironmentM)
  where
    clarifying = clarifyingLookDirectObjectM prep
-}
doLookTwoPrepM _ = throwError "doLookTwoPrep implementation unfinished"

-- FIXME . change data Object to data Entity everywhere
