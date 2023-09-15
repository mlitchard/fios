{-# OPTIONS_GHC -Wno-type-defaults #-}
module HauntedHouse.Game.Engine.VerbPhraseThree.Look where
import HauntedHouse.Recognizer.WordClasses (PrepPhrase(PrepPhrase1))
import HauntedHouse.Game.Model.World
import Control.Monad.Except (throwError)
import HauntedHouse.Game.Object (getObjectM)
import HauntedHouse.Game.Model.Display (updateDisplayActionM, showPlayerActionM, showEnvironmentM, maybeDescribeNexusM, updateContainerDescriptionM, updatePlayerActionM)
import HauntedHouse.Game.Engine.Verification (verifySensibilityNPPP)
import HauntedHouse.Clarifier (clarifyingLookDirectObjectM)

doLookTwoPrepM :: (PrepPhrase, PrepPhrase) -> GameStateExceptT ()
doLookTwoPrepM (PrepPhrase1 prep np,pp) = do
  gsub@(gid,_) <- verifySensibilityNPPP (clarifyingLookDirectObjectM prep) np pp
  updateContainerDescriptionM prep gsub
  (Object {..}) <- getObjectM gid
  case prep of 
    IN -> updatePlayerActionM ("You look at the " <> _shortName')
            >> (_lookIn' _standardAction')
  
  maybeDescribeNexusM _mNexus'
  updateDisplayActionM (showPlayerActionM >> showEnvironmentM)
doLookTwoPrepM _ = throwError "doLookTwoPrep implementation unfinished"

-- FIXME . change data Object to data Entity everywhere
-- FIXME set player action
-- FIXME what about entities that aren't a nexus?
