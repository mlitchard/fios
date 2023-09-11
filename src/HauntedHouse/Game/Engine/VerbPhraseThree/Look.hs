{-# OPTIONS_GHC -Wno-type-defaults #-}
module HauntedHouse.Game.Engine.VerbPhraseThree.Look where
import HauntedHouse.Recognizer.WordClasses (PrepPhrase(PrepPhrase1))
import HauntedHouse.Game.Model.World
import Control.Monad.Except (throwError)
import HauntedHouse.Game.Object (getObjectM)
import HauntedHouse.Game.Model.Display (updateDisplayActionM, showPlayerActionM, showEnvironmentM, maybeDescribeNexusM, updateContainerDescriptionM)
import HauntedHouse.Game.Engine.Verification (verifyExistenceNPPP)
import HauntedHouse.Clarifier (clarifyingLookDirectObjectM)

doLookTwoPrepM :: (PrepPhrase, PrepPhrase) -> GameStateExceptT ()
doLookTwoPrepM (PrepPhrase1 prep np,pp) = do
  gsub@(gid,_) <- verifyExistenceNPPP (clarifyingLookDirectObjectM prep) np pp
  updateContainerDescriptionM prep gsub
  updatedSubject <- getObjectM gid
  maybeDescribeNexusM (_mNexus' updatedSubject)
  updateDisplayActionM (showPlayerActionM >> showEnvironmentM)
doLookTwoPrepM _ = throwError "doLookTwoPrep implementation unfinished"

-- FIXME . change data Object to data Entity everywhere

