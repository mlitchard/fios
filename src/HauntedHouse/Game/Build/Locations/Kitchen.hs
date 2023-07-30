{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use one" #-}
module HauntedHouse.Game.Build.Locations.Kitchen where

import HauntedHouse.Game.Build.LocationTemplate
import HauntedHouse.Game.Build.Locations.BuildFrame
import HauntedHouse.Game.Build.Locations.Kitchen.Sink.Sink
import HauntedHouse.Game.Model (GameStateExceptT)
import HauntedHouse.Game.Build.Locations.Kitchen.ShelfArea.Shelf
import HauntedHouse.Game.Build.Locations.Kitchen.ShelfArea.Cabinets.AboveShelf
import HauntedHouse.Game.Build.Locations.Kitchen.ShelfArea.Cabinets.BelowShelf
import HauntedHouse.Game.Location (getLocation)

buildKitchen :: GameStateExceptT ()
buildKitchen = do
  location <- getLocation kitchenGID 
  buildFrame kitchenGID location
  buildKitchenSink
  buildKitchenShelf
  buildKitchenCabinetAboveShelf
  buildKitchenCabinetBelowShelf
