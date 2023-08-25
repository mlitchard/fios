module HauntedHouse.Game.Engine.OnlyVerb.DoLook where

import HauntedHouse.Game.Model.World -- (GameStateExceptT)
import HauntedHouse.Game.Narration (displaySceneM)

doLookM :: GameStateExceptT ()
doLookM = displaySceneM False  