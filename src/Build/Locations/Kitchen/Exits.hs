module Build.Locations.Kitchen.Exits where

import Game.Model.World
import Build.Locations.Kitchen.Exits.EastExit.Portal.MakePortal (kitchenEastPortal)
import Build.Locations.Kitchen.Exits.EastExit.Door.MakeDoor (kitchenEastDoor)

buildExits :: GameStateExceptT ()
buildExits =
  buildEastExit

buildEastExit :: GameStateExceptT ()
buildEastExit = do
  kitchenEastPortal
  kitchenEastDoor
 
