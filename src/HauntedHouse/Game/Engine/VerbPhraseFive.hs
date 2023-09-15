module HauntedHouse.Game.Engine.VerbPhraseFive where
import HauntedHouse.Recognizer.WordClasses (Verb, AdjPhrase)
import HauntedHouse.Game.Model.World 
import Control.Monad.Except (MonadError(..))

verbPhraseFive :: (Verb,  AdjPhrase) -> GameStateExceptT ()
-- verbPhraseFive (GET, ap) = do
  {-
  (_,entity@Object{..}) <- verifyAccessabilityAP ap
  let getM = _get' _standardActions'
  getM entity
  -}
verbPhraseFive _ = throwError ("verbPhraseFive not finished" :: Text)