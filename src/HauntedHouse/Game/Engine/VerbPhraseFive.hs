module HauntedHouse.Game.Engine.VerbPhraseFive where 
import HauntedHouse.Recognizer.WordClasses (Verb, AdjPhrase, NounPhrase)
import HauntedHouse.Game.Model.World (GameStateExceptT)
import HauntedHouse.Tokenizer.Data (Lexeme(..))

verbPhraseFive :: Verb -> AdjPhrase -> NounPhrase -> GameStateExceptT ()
verbPhraseFive GET ap np = pass 
verbPhraseFive _ _ _ = throwError ("verbPhraseFive not finished" :: Text)