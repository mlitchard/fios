module HauntedHouse.Game.Engine.VerbPhraseFive where
import HauntedHouse.Recognizer.WordClasses (Verb, AdjPhrase)
import HauntedHouse.Game.Model.World 
import HauntedHouse.Tokenizer.Data (Lexeme(..))
import Control.Monad.Except (MonadError(..))
import HauntedHouse.Game.Engine.Verification (verifyAccessabilityAP)

verbPhraseFive :: (Verb,  AdjPhrase) -> GameStateExceptT ()
verbPhraseFive (GET, ap) = do
  (_,entity@Object{..}) <- verifyAccessabilityAP ap
  print ("ready to continue" :: String)
  let getM = _get' _standardActions'
  getM entity
verbPhraseFive _ = throwError ("verbPhraseFive not finished" :: Text)