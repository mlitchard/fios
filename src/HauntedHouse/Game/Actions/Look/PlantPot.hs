module HauntedHouse.Game.Actions.Look.PlantPot where 
import HauntedHouse.Game.Model.World (GameStateExceptT)
import HauntedHouse.Game.Model.Display (updatePlayerActionM, updateEnvironmentM)

firstLook :: GameStateExceptT ()
firstLook = do
  updatePlayerActionM "You look at the empty plant pot"
  updateEnvironmentM "This plant pot needs some soil if a plant would grow in it."

lookOn :: GameStateExceptT ()
lookOn = do
  updateEnvironmentM ("On the bottom of the plant pot" 
                        <> "you see something etched into it"
                        <> " \"plant the pot plant in the plant pot.\" ")