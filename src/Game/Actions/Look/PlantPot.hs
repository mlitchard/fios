module Game.Actions.Look.PlantPot where 
import Game.Model.World (GameStateExceptT)
import Game.Model.Display (updatePlayerActionM, updateEnvironmentM)

firstLook :: GameStateExceptT ()
firstLook = do
  updatePlayerActionM "You look at the empty plant pot"
  updateEnvironmentM "This plant pot needs some soil if a plant would grow in it."

lookOn :: GameStateExceptT ()
lookOn = do
  updateEnvironmentM ("On the bottom of the plant pot" 
                        <> "you see something etched into it"
                        <> " \"plant the pot plant in the plant pot.\" ")