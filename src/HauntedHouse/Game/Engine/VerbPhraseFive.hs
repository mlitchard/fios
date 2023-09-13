module HauntedHouse.Game.Engine.VerbPhraseFive where
import HauntedHouse.Recognizer.WordClasses (Verb, AdjPhrase)
import HauntedHouse.Game.Model.World (GameStateExceptT)
import HauntedHouse.Tokenizer.Data (Lexeme(..))
import Control.Monad.Except (MonadError(..))
import HauntedHouse.Game.Engine.Verification (verifyAccessabilityAP)

verbPhraseFive :: (Verb,  AdjPhrase) -> GameStateExceptT ()
verbPhraseFive (GET, ap) = do
  _ <- verifyAccessabilityAP ap
  print ("ready to continue" :: String)
  pass
verbPhraseFive _ = throwError ("verbPhraseFive not finished" :: Text)