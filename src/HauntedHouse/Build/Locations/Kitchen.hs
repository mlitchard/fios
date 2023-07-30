{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use one" #-}
module HauntedHouse.Build.Locations.Kitchen where

import HauntedHouse.Build.LocationTemplate
import HauntedHouse.Build.Locations.BuildFrame
import HauntedHouse.Build.Locations.Kitchen.SinkArea.Sink
import HauntedHouse.Game.Model (GameStateExceptT)
import HauntedHouse.Build.Locations.Kitchen.ShelfArea.Shelf
import HauntedHouse.Build.Locations.Kitchen.ShelfArea.Cabinets.AboveShelf
import HauntedHouse.Build.Locations.Kitchen.ShelfArea.Cabinets.BelowShelf
import HauntedHouse.Game.Location (getLocation)

buildKitchen :: GameStateExceptT ()
buildKitchen = do
  location <- getLocation kitchenGID 
  buildFrame kitchenGID location
  buildKitchenSink
  buildKitchenShelf
  buildKitchenCabinetAboveShelf
  buildKitchenCabinetBelowShelf
