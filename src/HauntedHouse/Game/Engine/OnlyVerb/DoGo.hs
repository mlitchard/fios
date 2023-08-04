module HauntedHouse.Game.Engine.OnlyVerb.DoGo where
import HauntedHouse.Recognizer (NounPhrase (..))
import HauntedHouse.Game.Model (GameStateExceptT)
import Control.Monad.Except (throwError)
import HauntedHouse.Game.Model.Mapping (Label (..))
import HauntedHouse.Game.Model.World (Exit (..))
import qualified Data.HashSet
import qualified Data.List
import HauntedHouse.Tokenizer
import HauntedHouse.Game.World (getExitObjectM)
import HauntedHouse.Game.Player (setPlayerLocationM)

doGo :: NounPhrase -> GameStateExceptT ()
doGo (Noun mdir) = if isDirection mdir
                    then tryGo (Label mdir)
                    else throwError (show mdir <> " isn't a direction")
doGo _ = throwError "Can't do any fancy shit with Go yet"

isDirection :: Lexeme -> Bool
isDirection mdir = Data.List.elem mdir $ Data.HashSet.toList directions

tryGo :: Label Exit -> GameStateExceptT ()
tryGo _direction = do
  setPlayerLocationM . _toLocation' =<< getExitObjectM _direction
  


