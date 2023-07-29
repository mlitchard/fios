module HauntedHouse.Game.Build.Objects where



{-
-- xs' = map (Data.List.NonEmpty.fromList) 
--         $ ((groupBy (\(l,_) (l',_) -> l == l')) . sort) labelGIDPairs
initObj :: NonEmpty (ObjectLabel,GID Object)
            -> (ObjectLabel,[GID Object])
initObj xs@((objLabel,_) :| _) = (,) objLabel $ map snd $ toList xs

initLoc :: NonEmpty (LocationLabel,GID Location)
            -> (LocationLabel, [GID Location])
initLoc xs@((locLabel,_) :| _) = (,) locLabel $ map snd $ toList xs  

initObjectLabels :: [(ObjectLabel,GID Object)] -> [(ObjectLabel, [GID Object])]
initObjectLabels objectLabelPairs = map initObj grouped
  where
    grouped :: [Data.List.NonEmpty.NonEmpty (ObjectLabel, GID Object)]
    grouped = map Data.List.NonEmpty.fromList
            $ Data.List.groupBy (\(k,_) (k',_) -> k == k')
            $ sort objectLabelPairs

objectIDNames :: [(String,Lexeme)]
objectIDNames =
  [("kitchenSink", SINK)
  ,("kitchenCabinetBelowSink", CABINET)
  , ("kitchenCabinetAboveSink", CABINET)
  , ("kitchenShelf", SHELF)
  , ("kitchenCabinetAboveShelf",CABINET)
  , ("kitchenCabinetBelowShelf",CABINET)]

numberOfObjects :: [Integer]
numberOfObjects = [1 .. (toInteger $ length objectIDNames)]

objectNames :: [String]
objectNames = map fst objectIDNames
-}
{-

data InitState = InitState {
  _objects :: ObjectLabelMap 
  , _locations :: LocationMap
  , _world :: World
}

newtype ObjectLabelMap = ObjectLabelMap
  { _unObjectLabelMap :: Data.Map.Strict.Map ObjectLabel 
                                             (NonEmpty (GID Object))
  }

-}
{-
popObjectGID :: ObjectLabel -> InitStateT (GID Object)
popObjectGID oLabel = do
  omap' <- _unObjectLabelMap . _objectLabelMap' <$> get
  let mGid = Data.Map.Strict.lookup oLabel omap'
  case mGid of
    Nothing -> throwError ("could not find " <> show oLabel)
    Just (x :| xs) -> Data.NonEmpty.List.fromList xs
                        & (\xs' -> modify (`updateObject` xs'))
                        >> pure x
  where
    updateObject :: InitState -> Data.List.NonEmpty.NonEmpty (GID Object) -> InitState
    updateObject init'@(InitState (ObjectLabelMap objects) _ _ _ _ _) xs =
      let updatedMap = ObjectLabelMap $ Data.Map.Strict.insert oLabel xs objects
      in init'{_objectLabelMap' = updatedMap}

getObject :: GID Object -> InitStateT Object
getObject gid = 
-}