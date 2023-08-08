module HauntedHouse.Game.Narration where

import HauntedHouse.Game.Model
        (Narration (..), GameStateExceptT, GameState (..), Scene (..))
import HauntedHouse.Game.Model.World
        (Location (..), Objects (..), Object (..), Position (..)
        , Relations (..), Moveability (..) )
import HauntedHouse.Game.World (getObjectM)
import qualified Data.List.NonEmpty (filter, partition)
import Data.List.NonEmpty ((<|))
import qualified Data.List
import HauntedHouse.Tokenizer (objects)

updateNarration :: Narration -> GameStateExceptT ()
updateNarration narration =  modify' (\g -> g {_narration' = narration})

{-

data Location = Location
  { _title'           :: Text
  , _description'     :: Text
  , _anchoredObjects' :: Maybe AnchoredObjects
  , _floorInventory'  :: Maybe Objects
  , _directions'      :: Maybe ExitGIDMap
  } deriving stock Show

-}


displayScene :: Location -> GameStateExceptT ()
displayScene (Location title description anchored floor directions) = do
  liftIO $ print ("You are in the " <> title <> "\n")
  liftIO $ print (description <> "\n")
  liftIO $ print ("This is what you see\n" :: Text)
  --liftIO $ print $ maybe emptyRoom objectDescriptions objects
  where
    emptyRoom = "An Empty Room"