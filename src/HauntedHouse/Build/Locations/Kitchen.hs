{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use one" #-}
module HauntedHouse.Build.Locations.Kitchen where

import HauntedHouse.Build.LocationTemplate
import HauntedHouse.Build.Locations.BuildFrame
import HauntedHouse.Build.Locations.Kitchen.SinkArea.Sink
import HauntedHouse.Game.Model (GameStateExceptT)
import HauntedHouse.Game.Model.World (Location (_description'))
import HauntedHouse.Build.Locations.Kitchen.ShelfArea.Shelf
import HauntedHouse.Build.Locations.Kitchen.ShelfArea.Cabinets.AboveShelf
import HauntedHouse.Build.Locations.Kitchen.ShelfArea.Cabinets.BelowShelf
import HauntedHouse.Game.Location (getLocation)
import HauntedHouse.Build.LocationLabels (kitchenLabel)
buildKitchen :: GameStateExceptT ()
buildKitchen = do
  location <- (\loc -> loc{_description' = kitchenDescription}) 
                <$> getLocation kitchenGID 
  buildFrame kitchenGID kitchenLabel location
  buildKitchenSink
  buildKitchenShelf
  buildKitchenCabinetAboveShelf
  buildKitchenCabinetBelowShelf

kitchenDescription :: Text 
kitchenDescription = "It's a kitchen"