{-# OPTIONS_GHC -Wno-type-defaults #-}
module Game.Engine.VerbPhraseThree.Look where
import Game.Model.World
import Control.Monad.Except (throwError)
import Game.Engine.Verification
    ( identifyPossiblelObjects,
      matchAnchored,
      matchesProximity, evaluatePossibleObject, evaluatePossibleObjects, checkAdvObjProximity )
import Game.Model.Display (updateEnvironmentM, showEnvironmentM)
import Game.Engine.Verbs.Look (doLookObject)
import Game.Model.Mapping (Label(..))
import Recognizer (NounPhrase, PrepPhrase(PrepPhrase1))
import qualified Data.List.NonEmpty
import Game.Model.Condition (Proximity)
import Data.List.NonEmpty ((<|))
-- (_lookIn' _standardActions') _shortName'
-- (FoundObject,NonEmpty (FoundObject,Proximity)
data MatchResult
  = Adjectival (FoundObject,Proximity) -- adverbialObject found adjectivalObject proximity match
  | Adverbial (NonEmpty (FoundObject,Proximity)) -- adjectivalObject found adverbial(s)
  | ManyBoth (NonEmpty (FoundObject,NonEmpty (FoundObject,Proximity))) -- many adverbials found many adjectivals

doLookTwoPrepM :: (PrepPhrase, PrepPhrase) -> GameStateExceptT ()
doLookTwoPrepM  (PrepPhrase1 advPrep advNP,PrepPhrase1 adjPrep adjNP) = do
  (advosLabel,padvos') <- visibilityExistence advNP
  (adjosLable,padjos') <- visibilityExistence adjNP
  matchResult <- throwMaybeM errMsg =<< case padvos' of
      (padvo :| []) -> case padjos' of
                        (padjo :| []) -> do
                                          res <- matchAnchored padvo padjo
                                          case res of
                                            Nothing -> pure Nothing
                                            Just (advObj,_,proximity) -> pure
                                                            $ Just (Adjectival (advObj,proximity))
                        padjos        -> do
                                          res <- tryMatchAdv padvo (toList padjos)
                                          case res of
                                            Nothing -> pure Nothing
                                            Just (advObj,_,proximity) -> pure
                                                                $ Just (Adjectival (advObj, proximity))
      padvos -> case padjos' of
                  (padjo :| []) -> do
                                    res <- tryMatchAdj padjo (toList padvos)
                                    case res of
                                      Nothing -> pure Nothing
                                      Just match -> pure
                                                      $ Just (Adverbial match)
                  padjos -> do
                              res <- throwMaybeM errMsg
                                      =<< tryBoth (toList padvos) (toList padjos)
                              pure $ Just (ManyBoth res)
  case matchResult of
    (Adjectival (advObj, proximity)) -> if checkAdvObjProximity adjPrep proximity
                                          then doLookObject advPrep (_entity' advObj)
                                          else updateEnvironmentM notWhere
                                                >> showEnvironmentM
    (Adverbial advObjs) -> do
                              let res = Data.List.NonEmpty.filter
                                        (\(_,proximity) -> checkAdvObjProximity adjPrep proximity) advObjs
                              case res of
                                [] -> updateEnvironmentM notWhere
                                      >> showEnvironmentM
                                [(advObj,_)] -> do
                                                  doLookObject advPrep (_entity' advObj)
                                _xs -> throwError "descriptives case unhandled"

    ManyBoth _ -> throwError "ManyBoth unhandled"
  where
    notWhere = "That's not where you think it is."
    errMsg = "Seriously?"
    tryMatchAdv :: FoundObject
                    -> [FoundObject]
                    -> GameStateExceptT (Maybe (FoundObject,FoundObject,Proximity))
    tryMatchAdv _ [] = pure Nothing
    tryMatchAdv advObj [adjObj] = do
                                    res <- matchAnchored advObj adjObj
                                    case res of
                                      Nothing -> pure Nothing
                                      Just match -> pure (Just match)
    tryMatchAdv advObj (adjObj : adjObjXS) = do
      res <- matchAnchored advObj adjObj
      case res of
        Nothing -> tryMatchAdv advObj adjObjXS
        Just match -> pure (Just match)

    tryMatchAdj :: FoundObject
                    -> [FoundObject]                            -- advs
                    -> GameStateExceptT (Maybe (NonEmpty (FoundObject,Proximity)))
    tryMatchAdj adjObj advObjXS = do
      proximities' <- mapM (`matchAnchored` adjObj) advObjXS
      case catMaybes proximities' of
        [] -> pure Nothing
        proximities -> do
                        let removedAdj = (\(adv,_,proximity) -> (adv,proximity))
                                            <$> proximities
                        pure $ Just (Data.List.NonEmpty.fromList removedAdj)

    tryBoth :: [FoundObject]
                -> [FoundObject]
                -> GameStateExceptT (Maybe (NonEmpty (FoundObject,NonEmpty (FoundObject,Proximity))))
    tryBoth [] _ = pure Nothing
    tryBoth (advObj:advObvXS) adjObjXS = do
      res <- catMaybes <$> mapM  (matchAnchored advObj) adjObjXS
      case res of
        [] -> tryBoth advObvXS adjObjXS
        matches -> do
                      res' <- tryBoth advObvXS adjObjXS
                      let removeAdv = (\(_,adjObj,proximity) -> (adjObj,proximity))
                                        <$> matches
                      let newMatch = (,) advObj (Data.List.NonEmpty.fromList removeAdv)
                      pure $ (<|) newMatch <$> res'



{-
    tryBoth :: [FoundObject]
                -> [FoundObject]
                -> GameStateExceptT (Maybe (NonEmpty (FoundObject,FoundObject,Proximity)))
    tryBoth advObjXS adjObjXS = do
      tryBoth' advObjXS adjObjXS Nothing
      where
        tryBoth' :: [FoundObject]
                      -> [FoundObject]
                      -> Maybe (NonEmpty (FoundObject,FoundObject,Proximity))
                      -> GameStateExceptT (Maybe (NonEmpty (FoundObject,FoundObject,Proximity)))
        tryBoth' _ [] accum = pure accum
        tryBoth' [] _ accum = pure accum
        tryBoth' [advObj] [adjObv] (Just accum) =
          case matchObjects advObj adjObv of 
            Nothing -> pure (Just accum)
            Just (matchedAdj,proximity) -> pure (Just (accum |> (advObj,matchedAdj,proximity)))
        tryBoth' (advObv:advObvXS) [adjObj] accum =
          case matchObjects advObj adjObv of 
            Nothing -> tryBoth' advObvXS [adjObv] accum 
            Just (matchedAdj,proximity) -> pure (Just (accum |> (advObj,matchedAdj,proximity)))
        tryBoth' [advObv] (adjObv:adjObvXS) accum = 
          case matchObjects advObj adjObv of 
            Nothing -> tryBoth' [advObj] adjObvXS accum 
            Just (matchedAdj,proximity) -> pure (Just (accum |> (advObj,matchedAdj,proximity)))
        
        tryBoth' (advObj:advObjXS') (adjObv:adjObjXS') accum = do
          case matchObjects advObj adjObv of 
            Nothing -> tryBoth' advObjXS' adjObjXS' accum
            Just (matchedAdj,proximity) -> fmap (|>) accum   
-}

doLookTwoPrepM _ = throwError "doLooktwoPrepM incomplete"


{-
    tryBoth :: [FoundObject]
                -> [FoundObject]
                -> GameStateExceptT (Maybe (Proximity,Object))
    tryBoth [] _ = pure Nothing
    tryBoth (advObj : advObjXS) adjObjs = do
      res <- tryMatchAdv advObj adjObjs
      case res of
        Nothing -> tryBoth advObjXS adjObjs
        Just found -> pure (Just found)

    tryMatchAdj :: FoundObject
                    -> [FoundObject]
                    -> GameStateExceptT (Maybe (Proximity,Object))
    tryMatchAdj _ [] = pure Nothing
    tryMatchAdj adjObj (advObj : advObjXS) = do
      matchRes <- matchObjects advObj adjObj
      case matchRes of
        Nothing -> tryMatchAdj adjObj advObjXS
        Just proximity -> pure (Just (proximity, _entity' advObj))
        
          if matchesProximity proximity adjPrep
                            then doLookObject advPrep (_entity' advObj)
                            else updateEnvironmentM "That's not where you think it is."

    tryMatchAdv :: FoundObject
                    -> [FoundObject]
                    -> GameStateExceptT (Maybe (Proximity,Object))
    tryMatchAdv _ [] = pure Nothing
    tryMatchAdv advObj (adjObj : adjObjXS) = do
      matchRes <- matchObjects advObj adjObj
      case matchRes of
        Nothing -> tryMatchAdv advObj adjObjXS
        Just proximity -> pure (Just (proximity, _entity' advObj))
           if matchesProximity proximity adjPrep
                            then doLookObject advPrep (_entity' advObj)
                            else updateEnvironmentM "That's not where you think it is."

    tryDoObject :: FoundObject -> FoundObject -> GameStateExceptT ()
    tryDoObject advObj adjObj = do
      proximity <- throwMaybeM "That's not where you think it is"
                    =<< matchObjects advObj adjObj
      if matchesProximity proximity adjPrep
        then doLookObject advPrep (_entity' advObj)
        else updateEnvironmentM "That's not where you think it is."
              >> showEnvironmentM

doLookTwoPrepM _ = throwError "doLooktwoPrepM incomplete"
-}
   {- do
  padvo <- identifyPossiblelObjects advNP
  let padvoExistenceRes = case padvo of
                  (label, Nothing) -> Left (advObjNotFound label)
                  (label,Just found) -> Right (label, found)
  let padvoVisibilityRes = case padvoExistenceRes of
    (Left err) -> updateEnvironmentM err >> showEnvironmentM
    (Right found) -> do
      case found of
        (label,Found advObj') -> do
                                  advObj <- throwMaybeM (advObjNotFound label)
                                              $ evaluatePossibleObject advObj'
                                  
                                  pass

  pass
  where
    advObjNotFound (Label label) =
      "You don't see a " <> show label <> " here."
-}
visibilityExistence :: NounPhrase
                -> GameStateExceptT (Label Object,NonEmpty FoundObject)
visibilityExistence np = do
  padvo <- identifyPossiblelObjects np
  case padvo of
    (label, Nothing) -> throwError (objNotFound label)
    (label,Just (Found obj')) -> do
                                  obj <- throwMaybeM (objNotFound label)
                                          $ evaluatePossibleObject obj'
                                  pure $ (,) label (Data.List.NonEmpty.singleton obj)
    (label, Just (Possibles objs')) -> do
                                        objs <- throwMaybeM (objNotFound label)
                                                  $ evaluatePossibleObjects objs'
                                        pure (label,objs)

  where
    objNotFound (Label label) =
      "You don't see a " <> show label <> " here."

