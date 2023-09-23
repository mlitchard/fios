module Game.Engine.OnlyVerb.DoLook where

import Game.Model.World -- (GameStateExceptT)
import Game.Narration (displaySceneM)

doLookM :: GameStateExceptT ()
doLookM = displaySceneM False  