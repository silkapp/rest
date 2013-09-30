{-# LANGUAGE GADTs, CPP, ScopedTypeVariables #-}
module Rest.Gen.Base.ActionInfo where

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
import qualified Rest.Gen.Base.JSON as J
import qualified Rest.Gen.Base.XML as X

import Rest.Action
import Rest.Dictionary hiding (Ident)
import Rest.Driver.Routing (mkListHandler)
import Rest.Resource
import Rest.Schema

import qualified Rest.Dictionary as Dictionary
import qualified Rest.Resource   as Rest

--------------------
-- * The types describing a resource's actions.

-- | Representation of resource
type ResourceId  = [String]

-- | Intermediate data representation of Rest structure
data RequestMethod = GET | POST | PUT | DELETE deriving (Show, Eq)

data ActionType = Retrieve | Create | Delete | List | Update | Modify deriving (Show, Eq)

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

topLevelActionInfo :: Resource m s sid mid aid -> Cardinality sid mid -> [ActionInfo]
topLevelActionInfo r (Single _  ) = singleActionInfo r Nothing ""
topLevelActionInfo r (Many   mid) = return . listActionInfo Nothing "" . Rest.list r $ mid

stepActionInfo :: Resource m s sid mid aid -> Step sid mid aid -> [ActionInfo]
stepActionInfo r (Named hs) = concatMap (uncurry (namedActionInfo r)) hs
stepActionInfo r (Unnamed h) = unnamedActionInfo r h

namedActionInfo :: Resource m s sid mid aid -> String -> Either aid (Cardinality (Getter sid) (Getter mid)) -> [ActionInfo]
namedActionInfo r pth (Left aid) = [staticActionInfo Nothing pth (Rest.statics r aid)]
namedActionInfo r pth (Right (Single g)) = getterActionInfo     r pth g
namedActionInfo r pth (Right (Many   l)) = listGetterActionInfo r pth l

unnamedActionInfo :: Resource m s sid mid aid -> Cardinality (Id sid) (Id mid) -> [ActionInfo]
unnamedActionInfo r (Single (Id idnt _   )) = singleActionInfo r (Just $ actionIdent idnt) ""
unnamedActionInfo r (Many   (Id idnt midF)) = [listActionInfo (Just $ actionIdent idnt) "" (Rest.list r (midF listIdErr))]

getterActionInfo :: Resource m s sid mid aid -> String -> Getter sid -> [ActionInfo]
getterActionInfo r pth (Singleton _)    = singleActionInfo r Nothing                   pth
getterActionInfo r pth (By (Id idnt _)) = singleActionInfo r (Just $ actionIdent idnt) pth

listGetterActionInfo :: Resource m s sid mid aid -> String -> Getter mid -> [ActionInfo]
listGetterActionInfo r pth (Singleton mid)     = [listActionInfo Nothing                   pth (Rest.list r mid)]
listGetterActionInfo r pth (By (Id idnt midF)) = [listActionInfo (Just $ actionIdent idnt) pth (Rest.list r (midF listIdErr))]

listIdErr :: mid
listIdErr = error "Don't evaluate the fields of a list identifier unless in the body of the handler. They are undefined during generation of documentation and code."

singleActionInfo :: Resource m s sid mid aid -> Maybe Ident -> String -> [ActionInfo]
singleActionInfo r mIdent pth = foldMap (return . getActionInfo    mIdent pth) (Rest.get     r)
                             ++ foldMap (return . updateActionInfo mIdent pth) (Rest.update  r)
                             ++ foldMap (return . removeActionInfo mIdent    ) (Rest.remove  r)
                             ++ map     (uncurry selectActionInfo)             (Rest.selects r)
                             ++ map     (uncurry actionActionInfo)             (Rest.actions r)

--------------------
-- * Smart constructors for ActionInfo.

getActionInfo :: Maybe Ident -> String -> Handler m -> ActionInfo
getActionInfo mIdent pth = handlerActionInfo mIdent False Retrieve Self pth GET

updateActionInfo :: Maybe Ident -> String -> Handler m -> ActionInfo
updateActionInfo mIdent pth = handlerActionInfo mIdent False Update Any pth PUT

removeActionInfo :: Maybe Ident -> Handler m -> ActionInfo
removeActionInfo mIdent = handlerActionInfo mIdent True Delete Self "" DELETE

listActionInfo :: Maybe Ident -> String -> ListHandler m -> ActionInfo
listActionInfo mIdent pth = handlerActionInfo mIdent False List Self pth GET . mkListHandler

staticActionInfo :: Maybe Ident -> String -> Handler m -> ActionInfo
staticActionInfo mIdent pth = handlerActionInfo mIdent False Modify Any pth POST

createActionInfo :: Handler m -> ActionInfo
createActionInfo = handlerActionInfo Nothing False Create Self "" POST

selectActionInfo :: String -> Handler m -> ActionInfo
selectActionInfo pth = handlerActionInfo Nothing True Retrieve Any pth GET

actionActionInfo :: String -> Handler m -> ActionInfo
actionActionInfo pth = handlerActionInfo Nothing True Modify Any pth POST

handlerActionInfo :: Maybe Ident -> Bool -> ActionType -> ActionTarget -> String -> RequestMethod -> Handler m -> ActionInfo
handlerActionInfo mIdent postAct actType actTarget pth mth h = ActionInfo
  { ident        = mIdent
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
handlerParams (GenHandler (_, p, _, _, _) _ _) = paramNames p

paramNames :: Param a -> [String]
paramNames NoParam = []
paramNames (Param s _) = s
paramNames (TwoParams p1 p2) = paramNames p1 ++ paramNames p2

-- | Extract input description from handlers
handlerInputs :: Handler m -> [DataDescription]
handlerInputs (GenHandler (_, _, inps, _, _) _ _) = concatMap (handlerInput Proxy) inps
  where handlerInput :: Proxy a -> Input a -> [DataDescription]
        handlerInput _ NoI      = []
        handlerInput _ StringI  = [defaultDescription { dataTypeDesc = "String" }]
        handlerInput _ FileI    = [defaultDescription { dataType     = File
                                                      , dataTypeDesc = "File" }]
        handlerInput d ReadI    = [defaultDescription { dataTypeDesc = describe d }]
        handlerInput d XmlI     = [defaultDescription { dataType     = XML
                                                      , dataTypeDesc = "XML"
                                                      , dataSchema   = X.showSchema  . X.getXmlSchema $ d
                                                      , dataExample  = X.showExample . X.getXmlSchema $ d
                                                      , haskellType  = typeString d
                                                      , haskellModule = modString d
                                                      }]
        handlerInput _ XmlTextI = [defaultDescription { dataType     = XML
                                                      , dataTypeDesc = "XML"
                                                      , haskellType  = "String" }]
        handlerInput d JsonI    = [defaultDescription { dataType     = JSON
                                                      , dataTypeDesc = "JSON"
                                                      , dataExample  = J.showExample . J.schema $ d
                                                      , haskellType  = typeString d
                                                      , haskellModule = modString d
                                                      }]
        handlerInput _ RawXmlI  = [defaultDescription { dataType     = XML
                                                      , dataTypeDesc = "XML"
                                                      , haskellType  = "String"
                                                      }]

-- | Extract output description from handlers
handlerOutputs :: Handler m -> [DataDescription]
handlerOutputs (GenHandler (_, _, _, outps, _) _ _) = concatMap (handlerOutput Proxy) outps
  where handlerOutput :: Proxy a -> Output a -> [DataDescription]
        handlerOutput _ NoO      = []
        handlerOutput _ FileO    = [defaultDescription { dataType      = File
                                                       , dataTypeDesc  = "File" }]
        handlerOutput d XmlO     = [defaultDescription { dataType      = XML
                                                       , dataTypeDesc  = "XML"
                                                       , dataSchema    = X.showSchema  . X.getXmlSchema $ d
                                                       , dataExample   = X.showExample . X.getXmlSchema $ d
                                                       , haskellType   = typeString d
                                                       , haskellModule = modString d
                                                       }]
        handlerOutput d JsonO    = [defaultDescription { dataType      = JSON
                                                       , dataTypeDesc  = "JSON"
                                                       , dataExample   = J.showExample . J.schema $ d
                                                       , haskellType   = typeString d
                                                       , haskellModule = modString d
                                                       }]
        handlerOutput _ RawXmlO  = [defaultDescription { dataType     = XML
                                                       , dataTypeDesc = "XML"
                                                       , haskellType  = "String" }]
        handlerOutput _ StringO  = [defaultDescription { dataTypeDesc = "Text" }]

-- | Extract input description from handlers
handlerErrors :: Handler m -> [DataDescription]
handlerErrors (GenHandler (_, _, _, _, ers) _ _) = concatMap (handleError Proxy) ers
  where handleError :: Proxy a -> Error a -> [DataDescription]
        handleError _ NoE      = []
        handleError d XmlE     = [defaultDescription { dataType      = XML
                                                     , dataTypeDesc  = "XML"
                                                     , dataSchema    = X.showSchema  . X.getXmlSchema $ d
                                                     , dataExample   = X.showExample . X.getXmlSchema $ d
                                                     , haskellType   = typeString d
                                                     , haskellModule = modString d
                                                     }]
        handleError d JsonE    = [defaultDescription { dataType      = JSON
                                                     , dataTypeDesc  = "JSON"
                                                     , dataExample   = J.showExample . J.schema $ d
                                                     , haskellType   = typeString d
                                                     , haskellModule = modString d
                                                     }]
#if __GLASGOW_HASKELL__ >= 704
typeString :: Typeable a => a -> String
typeString = typeString' . typeOf
  where typeString' tr =
          let (tyCon, subs) = splitTyConApp tr
              showTyCon _ "[]" r = "[" ++ r ++ "]"
              showTyCon _ "()" _ = "()"
              showTyCon m d s | take 4 m == "GHC." = d ++ s
                              | otherwise = m ++ "." ++ d ++ s
          in  showTyCon (tyConModule tyCon) (tyConName tyCon) (concatMap (\t -> " (" ++ typeString' t ++ ")") subs)

modString :: Typeable a => a -> [String]
modString = filter (\v -> v /= "" && take 4 v /= "GHC.") . modString' . typeOf
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

actionIdent :: forall a. Dictionary.Ident a -> Ident
actionIdent StringId = Ident "string" "String" []
actionIdent ReadId   = Ident (describe proxy_) (typeString proxy_) (modString proxy_)
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
      Retrieve -> "Retrieve " ++ targetS ++ " data"
      Create   -> "Create " ++ targetS
      Delete   -> "Delete " ++ targetS
      List     -> "List " ++ targetS ++ "s"
      Update   -> "Update " ++ targetS
      Modify   -> "Modify " ++ targetS
