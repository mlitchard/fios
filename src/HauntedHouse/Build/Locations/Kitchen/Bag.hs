module HauntedHouse.Build.Locations.Kitchen.Bag where
import Data.Aeson ()
import HauntedHouse.Game.Model.World
        (Object(..), Orientation (ContainedBy'), ContainedBy (..), OnOrIn (In), StandardActions (..))
import HauntedHouse.Game.Model.Condition (Perceptibility(Imperceptible), Moveability (..))
import HauntedHouse.Build.ObjectTemplate (kitchenCabinetBelowShelfGID, bagOfSoilGID)
import HauntedHouse.Game.Model.Mapping (Label(..))
import HauntedHouse.Tokenizer (Lexeme(..))

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

bagOfSoil :: Object
bagOfSoil = Object
  { _shortName' = "a bag of soil"
  , _entityLabel' = Label BAG
  , _odescription' = mempty
  , _descriptives' = mempty
  , _moveability' = Moveable
  , _perceptability' = Imperceptible
  , _orientation' = containedByCabinet
  , _mNexus' = Nothing 
  , _standardActions' = standardActions
  }

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
