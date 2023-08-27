module HauntedHouse.Clarifier where

import Control.Monad.Except ( MonadError(throwError) )
import HauntedHouse.Game.Model.World (Object (..), GameStateExceptT, GameState (..))
import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Game.Model.Mapping (Label (..))
import HauntedHouse.Tokenizer (Lexeme)
import HauntedHouse.Game.Model.Display (describeOrientationM, updateEnvironmentM, showEnvironmentM, showPlayerActionM, updateDisplayActionM)
import Data.Text (toLower)

updateReport :: Text -> GameStateExceptT ()
updateReport  report = do
  currentReport <- _report' <$> get
  modify' (\gs -> gs{_report' = currentReport <> [report]})

clarifyWhich :: (Label Lexeme, NonEmpty Object) -> GameStateExceptT ()
clarifyWhich (Label label, objects) = 
  updateEnvironmentM preamble 
    >> mapM_ objectOrientation objects  
    >> updateDisplayActionM (showPlayerActionM >> showEnvironmentM)
  where
    preamble = "which " <> (toLower . toText) label <> " do you mean?"
{-
data Object = Object {
    _shortName'       :: Text
  , _odescription'    :: [Text]
  , _descriptives'    :: [Label Adjective]
  , _moveability'     :: Moveability
  , _perceptability'  :: Perceptibility
  , _orientation'     :: Orientation
  , _mNexus'          :: Maybe Nexus
}
-}
objectOrientation :: Object -> GameStateExceptT ()
objectOrientation (Object shortName _ _ _ _ orientation _) = 
  describeOrientationM ("The " <> shortName) orientation
{-

data Object = Object {
    _shortName'       :: Text
  , _odescription'    :: [Text]
  , _descriptives'    :: [Label Adjective]
  , _moveability'     :: Moveability
  , _perceptability'  :: Perceptibility
  , _mNexus'          :: Maybe Nexus
}

-}

clarifyNotThere :: GameStateExceptT ()
clarifyNotThere = throwError "You don't see that here"

