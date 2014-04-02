{-# LANGUAGE GADTs, CPP, ScopedTypeVariables #-}
module Rest.Gen.Base.ActionInfo where

import Prelude hiding (id, (.))

import Control.Applicative
import Control.Category
import Control.Monad
import Data.Foldable (foldMap)
import Data.List
import Data.Maybe
import Data.Proxy
import Data.Typeable
#if __GLASGOW_HASKELL__ < 704
import Data.List.Split
#endif
import Rest.Gen.Base.ActionInfo.Ident (Ident (Ident))
import Rest.Info
import qualified Data.JSON.Schema   as J
import qualified Data.Label.Total   as L
import qualified Rest.Gen.Base.JSON as J
import qualified Rest.Gen.Base.XML  as X

import Rest.Dictionary (Param (..), Input (..), Output (..), Error (..))
import Rest.Driver.Routing (mkListHandler, mkMultiPutHandler)
import Rest.Handler
import Rest.Resource
import Rest.Schema

import qualified Rest.Dictionary as Dict
import qualified Rest.Resource   as Rest

--------------------
-- * The types describing a resource's actions.

-- | Representation of resource
type ResourceId  = [String]

-- | Intermediate data representation of Rest structure
data RequestMethod = GET | POST | PUT | DELETE deriving (Show, Eq)

data ActionType = Retrieve | Create | Delete | List | Update | UpdateMany | Modify
  deriving (Show, Eq)

data ActionTarget = Self | Any deriving (Show, Eq)

data ActionInfo = ActionInfo
  { ident        :: Maybe Ident  -- Requires extra identifier in url? e.g. page/<identifier>
  , postAction   :: Bool         -- Works on identified resources? e.g. uri/<uri>/action
  , actionType   :: ActionType
  , actionTarget :: ActionTarget
  , resDir       :: String       -- Resource directory
  , method       :: RequestMethod
  , inputs       :: [DataDescription]
  , outputs      :: [DataDescription]
  , errors       :: [DataDescription]
  , params       :: [String]
  , https        :: Bool
  } deriving (Show, Eq)

isAccessor :: ActionInfo -> Bool
isAccessor ai = actionType ai == Retrieve && actionTarget ai == Self

data DataType = XML | JSON | File | Other deriving (Show, Eq)

-- | Description of input/output data
data DataDescription = DataDescription
  { dataType      :: DataType
  , dataTypeDesc  :: String
  , dataSchema    :: String
  , dataExample   :: String
  , haskellType   :: String
  , haskellModule :: [String]
  } deriving (Show, Eq)

defaultDescription :: DataDescription
defaultDescription = DataDescription Other "" "" "" "" []

chooseType :: [DataDescription] -> Maybe DataDescription
chooseType []         = Nothing
chooseType ls@(x : _) = Just $ fromMaybe x $ find ((JSON ==) . dataType) ls

--------------------
-- * Traverse a resource's Schema and Handlers to create a [ActionInfo].

resourceToActionInfo :: Resource m s sid mid aid -> [ActionInfo]
resourceToActionInfo r =
  case schema r of
    Schema mTopLevel step -> foldMap (topLevelActionInfo r) mTopLevel
                          ++ stepActionInfo r step
                          ++ foldMap (return . createActionInfo) (Rest.create r)
                          ++ foldMap (return . removeActionInfo) (Rest.remove  r)
                          ++ map (uncurry selectActionInfo) (Rest.selects r)
                          ++ map (uncurry actionActionInfo) (Rest.actions r)

topLevelActionInfo :: Resource m s sid mid aid -> Cardinality sid mid -> [ActionInfo]
topLevelActionInfo r            (Single _  ) = singleActionInfo r Nothing ""
topLevelActionInfo r@Resource{} (Many   mid) = maybeToList
                                             . listActionInfo Nothing ""
                                             . Rest.list r
                                             $ mid

stepActionInfo :: Resource m s sid mid aid -> Step sid mid aid -> [ActionInfo]
stepActionInfo r (Named hs) = concatMap (uncurry (namedActionInfo r)) hs
stepActionInfo r (Unnamed h) = unnamedActionInfo r h

namedActionInfo :: Resource m s sid mid aid -> String -> Endpoint sid mid aid -> [ActionInfo]
namedActionInfo r pth (Left aid) = [staticActionInfo pth (Rest.statics r aid)]
namedActionInfo r pth (Right (Single g)) = getterActionInfo     r pth g
namedActionInfo r pth (Right (Many   l)) = listGetterActionInfo r pth l

unnamedActionInfo :: Resource m s sid mid aid -> Cardinality (Id sid) (Id mid) -> [ActionInfo]
unnamedActionInfo r@Resource{} unnamed =
  case unnamed of
    Single id_             -> singleActionInfo r (Just id_) ""
    Many   id_@(Id _ midF) -> maybeToList $
      listActionInfo (Just id_) "" (Rest.list r (midF listIdErr))

getterActionInfo :: Resource m s sid mid aid -> String -> Getter sid -> [ActionInfo]
getterActionInfo r pth (Singleton _) = singleActionInfo r Nothing    pth
getterActionInfo r pth (By id_     ) = singleActionInfo r (Just id_) pth

listGetterActionInfo :: Resource m s sid mid aid -> String -> Getter mid -> [ActionInfo]
listGetterActionInfo r@Resource{} pth getter = maybeToList $
  case getter of
    Singleton mid      -> listActionInfo Nothing    pth (Rest.list r mid)
    By id_@(Id _ midF) -> listActionInfo (Just id_) pth (Rest.list r (midF listIdErr))

listIdErr :: mid
listIdErr = error $ "Don't evaluate the fields of a list identifier unless in the body of the handler. "
                 ++ "They are undefined during generation of documentation and code."

singleActionInfo :: Resource m s sid mid aid -> Maybe (Id sid) -> String -> [ActionInfo]
singleActionInfo r@Resource{} mId pth
   = foldMap (return . getActionInfo         mId pth) (Rest.get     r)
  ++ foldMap (return . updateActionInfo      mId pth) (Rest.update  r)
  ++ maybeToList (join $ multiUpdateActionInfo <$> mId <*> pure pth <*> Rest.update r)

--------------------
-- * Smart constructors for ActionInfo.

getActionInfo :: Maybe (Id sid) -> String -> Handler m -> ActionInfo
getActionInfo mId pth = handlerActionInfo mId False Retrieve Self pth GET

updateActionInfo :: Maybe (Id sid) -> String -> Handler m -> ActionInfo
updateActionInfo mId pth = handlerActionInfo mId False Update Any pth PUT

multiUpdateActionInfo :: Monad m => Id sid -> String -> Handler m -> Maybe ActionInfo
multiUpdateActionInfo id_ pth h =  handlerActionInfo Nothing False UpdateMany Any pth PUT
                               <$> mkMultiPutHandler id_ (const id) h

removeActionInfo :: Handler m -> ActionInfo
removeActionInfo = handlerActionInfo Nothing True Delete Self "" DELETE

listActionInfo :: Monad m => Maybe (Id mid) -> String -> ListHandler m -> Maybe ActionInfo
listActionInfo mId pth h = handlerActionInfo mId False List Self pth GET <$> mkListHandler h

staticActionInfo :: String -> Handler m -> ActionInfo
staticActionInfo pth = handlerActionInfo Nothing False Modify Any pth POST

createActionInfo :: Handler m -> ActionInfo
createActionInfo = handlerActionInfo Nothing False Create Self "" POST

selectActionInfo :: String -> Handler m -> ActionInfo
selectActionInfo pth = handlerActionInfo Nothing True Retrieve Any pth GET

actionActionInfo :: String -> Handler m -> ActionInfo
actionActionInfo pth = handlerActionInfo Nothing True Modify Any pth POST

handlerActionInfo :: Maybe (Id id)
                  -> Bool
                  -> ActionType
                  -> ActionTarget
                  -> String
                  -> RequestMethod
                  -> Handler m
                  -> ActionInfo
handlerActionInfo mId postAct actType actTarget pth mth h = ActionInfo
  { ident        = idIdent <$> mId
  , postAction   = postAct
  , actionType   = actType
  , actionTarget = actTarget
  , resDir       = pth
  , method       = mth
  , inputs       = handlerInputs  h
  , outputs      = handlerOutputs h
  , errors       = handlerErrors  h
  , params       = handlerParams  h
  , https        = secure         h
  }

--------------------
-- * Utilities for extraction information from Handlers.

handlerParams :: GenHandler m f -> [String]
handlerParams (GenHandler dict _ _) = paramNames (L.get Dict.params dict)

-- | A `Param` can contain the same parameter multiple times. For
-- example, 'offset' and 'count' are added in Rest.Handler.mkListing,
-- and in Rest.Driver.Routing.mkListHandler. For that reason, we nub
-- here.

paramNames :: Param a -> [String]
paramNames = nub . paramNames_

paramNames_ :: Param a -> [String]
paramNames_ NoParam = []
paramNames_ (Param s _) = s
paramNames_ (TwoParams p1 p2) = paramNames p1 ++ paramNames p2

-- | Extract input description from handlers
handlerInputs :: Handler m -> [DataDescription]
handlerInputs (GenHandler dict _ _) = map (handlerInput Proxy) (L.get (Dict.dicts . Dict.inputs) dict)
  where handlerInput :: Proxy a -> Input a -> DataDescription
        handlerInput d ReadI    = defaultDescription { dataTypeDesc = describe d }
        handlerInput _ StringI  = defaultDescription { dataTypeDesc = "String" }
        handlerInput d XmlI     = defaultDescription { dataType     = XML
                                                     , dataTypeDesc = "XML"
                                                     , dataSchema   = X.showSchema  . X.getXmlSchema $ d
                                                     , dataExample  = X.showExample . X.getXmlSchema $ d
                                                     , haskellType  = typeString d
                                                     , haskellModule = modString d
                                                     }
        handlerInput _ XmlTextI = defaultDescription { dataType     = XML
                                                     , dataTypeDesc = "XML"
                                                     , haskellType  = "String"
                                                     }
        handlerInput _ RawXmlI  = defaultDescription { dataType     = XML
                                                     , dataTypeDesc = "XML"
                                                     , haskellType  = "String"
                                                     }
        handlerInput d JsonI    = defaultDescription { dataType     = JSON
                                                     , dataTypeDesc = "JSON"
                                                     , dataExample  = J.showExample . J.schema $ d
                                                     , haskellType  = typeString d
                                                     , haskellModule = modString d
                                                     }
        handlerInput _ FileI    = defaultDescription { dataType     = File
                                                     , dataTypeDesc = "File"
                                                     }

-- | Extract output description from handlers
handlerOutputs :: Handler m -> [DataDescription]
handlerOutputs (GenHandler dict _ _) = map (handlerOutput Proxy) (L.get (Dict.dicts . Dict.outputs) dict)
  where handlerOutput :: Proxy a -> Output a -> DataDescription
        handlerOutput _ StringO  = defaultDescription { dataTypeDesc = "Text" }
        handlerOutput d XmlO     = defaultDescription { dataType      = XML
                                                      , dataTypeDesc  = "XML"
                                                      , dataSchema    = X.showSchema  . X.getXmlSchema $ d
                                                      , dataExample   = X.showExample . X.getXmlSchema $ d
                                                      , haskellType   = typeString d
                                                      , haskellModule = modString d
                                                      }
        handlerOutput _ RawXmlO  = defaultDescription { dataType     = XML
                                                      , dataTypeDesc = "XML"
                                                      , haskellType  = "String"
                                                      }
        handlerOutput d JsonO    = defaultDescription { dataType      = JSON
                                                      , dataTypeDesc  = "JSON"
                                                      , dataExample   = J.showExample . J.schema $ d
                                                      , haskellType   = typeString d
                                                      , haskellModule = modString d
                                                      }
        handlerOutput _ FileO    = defaultDescription { dataType      = File
                                                      , dataTypeDesc  = "File"
                                                      }

-- | Extract input description from handlers
handlerErrors :: Handler m -> [DataDescription]
handlerErrors (GenHandler dict _ _) = map (handleError Proxy) (L.get (Dict.dicts . Dict.errors) dict)
  where handleError :: Proxy a -> Error a -> DataDescription
        handleError d XmlE     = defaultDescription { dataType      = XML
                                                    , dataTypeDesc  = "XML"
                                                    , dataSchema    = X.showSchema  . X.getXmlSchema $ d
                                                    , dataExample   = X.showExample . X.getXmlSchema $ d
                                                    , haskellType   = typeString d
                                                    , haskellModule = modString d
                                                    }
        handleError d JsonE    = defaultDescription { dataType      = JSON
                                                    , dataTypeDesc  = "JSON"
                                                    , dataExample   = J.showExample . J.schema $ d
                                                    , haskellType   = typeString d
                                                    , haskellModule = modString d
                                                    }
#if __GLASGOW_HASKELL__ >= 704
typeString :: forall a. Typeable a => Proxy a -> String
typeString _ = typeString' . typeOf $ (undefined :: a)
  where typeString' tr =
          let (tyCon, subs) = splitTyConApp tr
              showTyCon _ "[]" r = "[" ++ r ++ "]"
              showTyCon _ "()" _ = "()"
              showTyCon m d s | take 4 m == "GHC." = d ++ s
                              | otherwise = m ++ "." ++ d ++ s
          in  showTyCon (tyConModule tyCon)
                        (tyConName tyCon)
                        (concatMap (\t -> " (" ++ typeString' t ++ ")") subs)

modString :: forall a. Typeable a => Proxy a -> [String]
modString _ = filter (\v -> v /= "" && take 4 v /= "GHC.") . modString' . typeOf $ (undefined :: a)
  where modString' tr =
          let (tyCon, subs) = splitTyConApp tr
          in  tyConModule tyCon : concatMap modString' subs
#else
typeString :: Typeable a => a -> String
typeString = show . typeOf

modString :: Typeable a => a -> [String]
modString = filter (/= "") . modString' . typeOf
  where modString' tr =
          let (tyCon, subs) = splitTyConApp tr
          in (intercalate "." . init . splitOn "." . tyConString $ tyCon) : concatMap modString' subs
#endif

idIdent :: Id id -> Ident
idIdent (Id idnt _) = actionIdent idnt

actionIdent :: forall a. Dict.Ident a -> Ident
actionIdent Dict.StringId = Ident "string" "String" []
actionIdent Dict.ReadId   = Ident (describe proxy_) (typeString proxy_) (modString proxy_)
  where
    proxy_ :: Proxy a
    proxy_ = Proxy

mkActionDescription :: String -> ActionInfo -> String
mkActionDescription res ai =
  let targetS =
        case actionTarget ai of
          Self -> res
          Any  -> "information"
  in case actionType ai of
      Retrieve   -> "Retrieve " ++ targetS ++ " data"
      Create     -> "Create " ++ targetS
      Delete     -> "Delete " ++ targetS
      List       -> "List " ++ targetS ++ "s"
      Update     -> "Update " ++ targetS
      UpdateMany -> "Update many " ++ targetS
      Modify     -> "Modify " ++ targetS
