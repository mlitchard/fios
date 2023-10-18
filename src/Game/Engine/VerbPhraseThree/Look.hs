{-# OPTIONS_GHC -Wno-type-defaults #-}
module Game.Engine.VerbPhraseThree.Look where
import Recognizer.WordClasses (PrepPhrase(PrepPhrase1))
import Game.Model.World
import Control.Monad.Except (throwError)
import Game.Engine.Verification
    ( identifyPossiblelObjects,
      matchObjects,
      matchesProximity )
import Game.Model.Display (updateEnvironmentM)
import Game.Engine.Verbs.Look (whichLook)
-- (_lookIn' _standardActions') _shortName'
doLookTwoPrepM :: (PrepPhrase, PrepPhrase) -> GameStateExceptT ()
doLookTwoPrepM (PrepPhrase1 advPrep advNP,PrepPhrase1 adjPrep adjNP) = do
  padvdo <- identifyPossiblelObjects advNP
  case padvdo of
    (label,Nothing) -> throwError ("You don't see that here")
    (label,Just (Found advObj)) -> do
                  -- evaluatePossibleObject 
                  padjo <- identifyPossiblelObjects adjNP
                  case padjo of
                    (_,Nothing) -> updateEnvironmentM "That makes no sense" >> pass    
                    (label', Just (Found adjObj)) -> do
                                                res <- matchObjects advObj adjObj
                                                case res of
                                                  Nothing -> updateEnvironmentM ("You don't see the " <> show label' <> "there")
                                                  (Just proximity)
                                                    | matchesProximity proximity adjPrep -> pass -- whichLook advPrep advObj
                                                    | otherwise -> updateEnvironmentM ("You don't see the " <> show label' <> "there")
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
