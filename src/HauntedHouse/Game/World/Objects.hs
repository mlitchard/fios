module HauntedHouse.Game.World.Objects where
import HauntedHouse.Game.GID (GID (..))
import HauntedHouse.Game.Object.Atomic (ObjectLabel)

initOrg :: NonEmpty (ObjectLabel,GID ObjectLabel) 
            -> (ObjectLabel,[GID ObjectLabel]) 
initOrg xs@((objLabel,_) :| _) = (,) objLabel $ map snd $ toList xs   
