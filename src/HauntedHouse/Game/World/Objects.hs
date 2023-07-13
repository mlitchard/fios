module HauntedHouse.Game.World.Objects where
import HauntedHouse.Game.GID (GID (..))
import HauntedHouse.Game.Object.Container (ObjectLabel)

type ContainedBy = GID Label
type Containing  = [GID ObjectLabel]
