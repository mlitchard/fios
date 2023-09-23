module Game.Engine.VerbPhraseFive where
import Recognizer.WordClasses (Verb, AdjPhrase)
import Game.Model.World 
import Control.Monad.Except (MonadError(..))

verbPhraseFive :: (Verb,  AdjPhrase) -> GameStateExceptT ()
-- verbPhraseFive (GET, ap) = do
  {-
  (_,entity@Object{..}) <- verifyAccessabilityAP ap
  let getM = _get' _standardActions'
  getM entity
  -}
verbPhraseFive _ = throwError ("verbPhraseFive not finished" :: Text)