module Build.Locations.Kitchen.Bag where
import Data.Aeson ()
import Game.Model.World
        (Object(..), Orientation (ContainedBy'), ContainedBy (..), OnOrIn (In), StandardActions (..))
import Game.Model.Condition (Perceptibility(Imperceptible), Moveability (..))
import Build.ObjectTemplate (kitchenCabinetBelowShelfGID, bagOfSoilGID)
import Game.Model.Mapping (Label(..))
import Tokenizer (Lexeme(..))

{-
plantPot :: Object
plantPot = Object {
      _shortName'      = "plant pot"
    , _entityLabel' = Label POT
    , _odescription'   = [desc]
    , _descriptives'   = [Label PLANT, Label SMALL]
    , _moveability'    = NotMoveable
    , _perceptability' = Perceptible
    , _orientation'    = floorOrientation -- orientation 
    , _mNexus'         = Nothing
    , _standardActions' = standardActions
  }
  where
    desc = "You can plant plants in the plant pot."
-}
{-
bagOfSoil :: Object
bagOfSoil = Object
  { _shortName' = "a bag of soil"
  , _entityLabel' = Label BAG
  , _odescription' = mempty
  , _descriptives' = mempty
  , _moveability' = Moveable
  , _perceptability' = Imperceptible
  , _orientation' = containedByCabinet
  , _mNexus' = Just inBag 
  , _standardActions' = standardActions
  }

inBag :: Nexus 
inBag = Container' . This containedIn 

containedIn = ContainedIn 
  {_containerInterface' = bagInterface 
  , }
{-
data ContainedIn = ContainedIn
  { _containerInterface'  :: ContainerInterface
  , _containedIn'         :: ContainerMap Object
  }
-}
containedByCabinet :: Orientation
containedByCabinet = ContainedBy' $ ContainedBy {
      _containedBy' = In kitchenCabinetBelowShelfGID
    , _self' = bagOfSoilGID
  }

standardActions :: StandardActions 
standardActions = StandardActions
  { _get' = const pass
  , _put' = const pass
  , _lookIn' = const pass
  , _lookAt' = const pass
  , _lookOn' = const pass 
  }
-}