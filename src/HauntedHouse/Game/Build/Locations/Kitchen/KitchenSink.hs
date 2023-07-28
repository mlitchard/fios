module HauntedHouse.Game.Build.Locations.Kitchen.KitchenSink where

{-
kitchenSink 
kitchenSink = Object
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

makeSink :: InitStateT ()
makeSink = do 
  makeSink' 
  makeUpperSinkCabinet
  makeLowerSinkCabinet 
  where 
    makeSink' :: InitStateT ()
    makeSink' = pass

makeUpperSinkCabinet :: InitStateT () 
makeUpperSinkCabinet = pass 

makeLowerSinkCabinet :: InitStateT ()
makeLowerSinkCabinet = pass 
-}