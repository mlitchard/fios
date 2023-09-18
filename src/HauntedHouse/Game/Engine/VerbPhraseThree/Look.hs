{-# OPTIONS_GHC -Wno-type-defaults #-}
module HauntedHouse.Game.Engine.VerbPhraseThree.Look where
import HauntedHouse.Recognizer.WordClasses (PrepPhrase(PrepPhrase1))
import HauntedHouse.Game.Model.World
import Control.Monad.Except (throwError)
import HauntedHouse.Game.Model.Display
        (updateDisplayActionM, showPlayerActionM, showEnvironmentM
        , updateContainerDescriptionM, updatePlayerActionM)
import HauntedHouse.Game.Engine.Verification (verifySensibilityNPPP, containerTest)
import HauntedHouse.Clarifier (clarifyingLookDirectObjectM)
import HauntedHouse.Tokenizer (Lexeme(ON, IN, AT))
-- (_lookIn' _standardActions') _shortName'
doLookTwoPrepM :: (PrepPhrase, PrepPhrase) -> GameStateExceptT ()
doLookTwoPrepM (PrepPhrase1 prep np,pp) = do
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
doLookTwoPrepM _ = throwError "doLookTwoPrep implementation unfinished"

-- FIXME . change data Object to data Entity everywhere
