{-# OPTIONS_GHC -Wno-type-defaults #-}
module Game.Engine.VerbPhraseThree.Look where
import Game.Model.World
    ( throwMaybeM,
      Object(_shortName'),
      FoundObject(FoundObject, _entity', _gid'),
      GameStateExceptT )
import Control.Monad.Except (throwError)
import Game.Engine.Verification
    ( matchAnchored, checkAdvObjProximity, visibilityExistence )
import Game.Model.Display (updateEnvironmentM, showEnvironmentM)
import Game.Engine.Verbs.Look (doLookObject)
import Recognizer (PrepPhrase(PrepPhrase1))
import qualified Data.List.NonEmpty
import Game.Model.Condition (Proximity)
import Data.List.NonEmpty ((<|))

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

doLookTwoPrepM _ = throwError "doLooktwoPrepM incomplete"