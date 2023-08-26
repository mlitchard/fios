module HauntedHouse.Clarifier where

import Control.Monad.Except ( MonadError(throwError) )
import HauntedHouse.Game.Model.World (Object, GameStateExceptT, GameState (..))
import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Game.Model.Mapping (Label (..))
import HauntedHouse.Tokenizer (Lexeme)

clarifier :: Text -> GameStateExceptT ()
clarifier report = do
  currentReport <- _report' <$> get
  modify' (\gs -> gs{_report' = currentReport <> [report]})

clarifyWhich :: (Label Lexeme, NonEmpty (GID Object)) -> GameStateExceptT ()
clarifyWhich (Label label, objectGids) = do
  whichObjectList <- mapM whichObjectM objectGids
  pass
  where
    preamble = "which " <> toText label <> " do you mean?"

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
whichObjectM :: GID Object -> GameStateExceptT Text 
whichObjectM gid = pure mempty 

clarifyNotThere :: GameStateExceptT ()
clarifyNotThere = throwError "You don't see that here"

