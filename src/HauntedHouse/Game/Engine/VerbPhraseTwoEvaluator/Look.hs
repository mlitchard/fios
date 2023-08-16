module HauntedHouse.Game.Engine.VerbPhraseTwoEvaluator.Look where
import HauntedHouse.Recognizer.WordClasses (PrepPhrase (..), NounPhrase (..))
import HauntedHouse.Game.Model (GameStateExceptT)
import HauntedHouse.Tokenizer (Lexeme (..))
import HauntedHouse.Game.Model.World (Location(..))
import Control.Monad.Except (throwError)
import HauntedHouse.Game.Location (getLocationM, getLocationIdM)
import HauntedHouse.Game.Model.Mapping
    ( LabelToGIDListMapping(_unLabelToGIDListMapping'), Label (..) )
import qualified Data.Map.Strict (lookup)
import qualified Data.List.NonEmpty
import HauntedHouse.Internal ( throwMaybeM )
import Relude.Extra (toFst, fmapToFst)

doLookObjectM :: PrepPhrase -> GameStateExceptT ()
doLookObjectM (PrepPhrase AT np ) = evaluateATNounPhrase np
doLookObjectM (PrepPhrase IN _)  = pass
doLookObjectM (PrepPhrase p _)   =
  throwError (show p <> " not evaulated in doLookObjectM" :: Text)
doLookObjectM (Preposition p) = throwError ("simple preposition "
                                  <> show p <> "not evaluated in doLookObjectM")

{-

data NounPhrase
  = NounPhrase1 Determiner NounPhrase
  | NounPhrase2 Determiner AdjPhrase NounPhrase
  | NounPhrase3 Number NounPhrase
  | NounPhrase4 NounPhrase PrepPhrase
  | NounPhrase5 AdjPhrase NounPhrase
  | Noun Noun

-}
evaluateATNounPhrase :: NounPhrase -> GameStateExceptT ()
evaluateATNounPhrase (Noun noun) = do
  labelMap <- _unLabelToGIDListMapping' . _objectLabelMap'
                <$> (getLocationM =<< getLocationIdM)
  (_l,objectList) <- fmapToFst length
                      $ throwMaybeM "nope" 
                      $ Data.Map.Strict.lookup (Label noun) labelMap
  -- if | l == 0 clarifyNotThere
  print (("There are " <> (show . Data.List.NonEmpty.length $ objectList)) :: Text)

evaluateATNounPhrase _ = throwError "evaluateATNounPhrase incomplete"