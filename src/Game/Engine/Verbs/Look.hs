module Game.Engine.Verbs.Look where
import Game.Model.GID (GID)
import Game.Model.World
    ( StandardActions(_lookAction'),
      Object(..),
      LookFunctions(_lookOn', _lookAt', _lookIn'),
      LookAction(_lookFunctions'),
      LookInF(_unLookIn'),
      LookOnF(_unLookOn'),
      LookAtF(_unLookAt'),
      GameStateExceptT )
import Recognizer (Preposition)
import Tokenizer (Lexeme (..))
import Control.Monad.Except (MonadError(throwError))
import Game.Actions.Look.StandardLook (look)
import Game.Model.Display (updateDisplayActionM, showPlayerActionM, showEnvironmentM)

doLookObject :: Lexeme -> Object -> GameStateExceptT ()
doLookObject prep entity@(Object {..})= do
  let lookf =  case prep of
            AT -> lookFunctions._lookAt'._unLookAt'
            IN -> lookFunctions._lookIn'._unLookIn'
            ON -> lookFunctions._lookOn'._unLookOn'
            _ -> const (const (throwError lookAbsurd))
  look entity lookf
  updateDisplayActionM (showPlayerActionM >> showEnvironmentM)
  pass
  where
    lookAbsurd = "Think hard about what you just tried to do."
    lookFunctions = _standardActions'._lookAction'._lookFunctions'

whichLook :: Preposition -> GID Object -> GameStateExceptT ()
whichLook _ _ = pass