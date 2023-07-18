module HauntedHouse.Game.World.Locations.Kitchen.KitchenSink where
import HauntedHouse.Game.Object
import HauntedHouse.Game.World.InitState (InitState)
import HauntedHouse.Game.Object.Container.Domain (AttachedTo
                                                  , Containing
                                                  , RelatedObjects
                                                  , Container (..), Moveable (NotMovable))

kitchenSink :: Maybe AttachedTo -> Containing -> RelatedObjects -> Object
kitchenSink containedBy containing relatedObjects = Object
  {_container = Just container
  , _containedBy = containedBy
  , _moveability = NotMovable
  , _odescription = "A Kitchen sink. It has a cabinet above, and a cabinet below."
  }
    where 
      container = Container
        { _isOpen = Just True 
        , _containing = containing 
        , _lockState = Nothing 
        , _relatedObjects = relatedObjects 
        }

makeSink :: InitState ()
makeSink = do 
  makeSink' 
  makeUpperSinkCabinet
  makeLowerSinkCabinet 
  where 
    makeSink' :: InitState ()
    makeSink' = pass

makeUpperSinkCabinet :: InitState () 
makeUpperSinkCabinet = pass 

makeLowerSinkCabinet :: InitState ()
makeLowerSinkCabinet = pass 