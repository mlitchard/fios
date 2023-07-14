module HauntedHouse.Game.World.Locations.Kitchen.KitchenSink where

import Data.These (These (..))
import HauntedHouse.Game.Object

import HauntedHouse.Game.GID (GID(..))
import HauntedHouse.Tokenizer (Lexeme(..))
import HauntedHouse.Game.Location
import HauntedHouse.Game.World.Objects
import HauntedHouse.Game.Object.Atomic (ObjectLabel (..))
import HauntedHouse.Game.World.WorldState (WorldState)

kitchenSinkName :: ObjectLabel
kitchenSinkName = ObjectLabel SINK

{-

  { _container :: Maybe ContainerState 
  , _containedBy :: Maybe AttachedTo 
  , _moveability :: Moveable
  , _odescription :: Text
  }

-}

kitchenSink' :: GID LocationLabel -> Containing -> Object
kitchenSink' containedBy containing = Object
  { _container = Just kitchenSinkContainerState
  , _containedBy = Just (AttachedToLocation containedBy)
  , _moveability = NotMovable
  , _odescription =
      "A kitchen sink."
        <> " There is a cabinet above the sink."
        <> " There is a cabinet below the sink"
        <> " There is a shelf to the right of the sink."
  }
  where
    kitchenSinkContainerState :: ContainerState
    kitchenSinkContainerState = ContainerState
      (This kitchenSinkAsContainer)

    kitchenSinkAsContainer :: Container
    kitchenSinkAsContainer = Container
      { _isOpen = Nothing -- Can't open or close a sink 
      , _cinv = containing
      , _lockState = Nothing -- Can't lock a sink
      }
    {-
     kitchenSinkAsShelf :: Shelf
     kitchenSinkAsShelf = Shelf 
      {

      -}

makeSink :: WorldState 
makeSink = do 
  makeSink' 
  makeUpperSinkCabinet
  makeLowerSinkCabinet 
  where 
    makeSink' :: WorldState 
    makeSink' = pass

makeUpperSinkCabinet :: WorldState 
makeUpperSinkCabinet = pass 

makeLowerSinkCabinet :: WorldState 
makeLowerSinkCabinet = pass 