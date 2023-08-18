module HauntedHouse.Game.Narration.Anchored where

import qualified Data.Text
import HauntedHouse.Game.Model.World (RoomAnchor)

directionFromRoomAnchor :: RoomAnchor -> Text
directionFromRoomAnchor roomAnchor = 
  Data.Text.toLower . fst $ Data.Text.breakOn "Anchor" (show roomAnchor)

