{-# LANGUAGE GADTs, CPP #-}
module Rest.Gen.Base.ActionInfo where

import Data.List
import Data.Maybe
import Data.Typeable
#if __GLASGOW_HASKELL__ < 704
import Data.List.Split
#endif
import Rest.Gen.Base.ActionInfo.Ident (Ident (Ident))
import Rest.ReadInfo
import qualified Rest.Gen.Base.JSON as J
import qualified Rest.Gen.Base.XML as X

import Rest.Resource
import Rest.Action hiding (Ident, ident)
import qualified Rest.Action as Action

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

resourceToActionInfo :: Resource m s a -> [ActionInfo]
resourceToActionInfo r =
  let
    hId :: Handler m a -> Maybe Ident
    hId = handlerIdent
    aId :: Action m -> Maybe Ident
    aId (Action h) = hId h

    hInp :: Handler m a -> [DataDescription]
    hInp =  handlerInputs
    aInp :: Action m -> [DataDescription]
    aInp (Action h) = hInp h

    hOut :: Bool -> Handler m a -> [DataDescription]
    hOut =  handlerOutputs
    aOut :: Bool -> Action m -> [DataDescription]
    aOut b (Action h) = hOut b h

    hErr :: Handler m a -> [DataDescription]
    hErr =  handlerErrors
    aErr :: Action m -> [DataDescription]
    aErr (Action h) = hErr h

    hPar :: Handler m a -> [String]
    hPar (Handler (_, _, pars, _, _, _) _ _ _) =
      case pars of
        NoParam      -> []
        (Param s _) -> s

    aPar :: Action m -> [String]
    aPar (Action h) = hPar h

    hSec :: Handler m a -> Bool
    hSec = secure

    aSec :: Action m -> Bool
    aSec (Action h) = hSec h

    sGet      = map (\(s, h) ->            ActionInfo (hId h)  False Retrieve Self s  GET    (hInp h)  (hOut False h)  (hErr h)  (hPar h)  (hSec h))  $ singleGet r
    sGetBy    = map (\(s, h) ->            ActionInfo (hId h)  False Retrieve Self s  GET    (hInp h)  (hOut False h)  (hErr h)  (hPar h)  (hSec h))  $ singleGetBy r
    sCreate   = maybeToList . fmap (\ac -> ActionInfo (aId ac) False Create   Self "" POST   (aInp ac) (aOut False ac) (aErr ac) (aPar ac) (aSec ac)) $ singleCreate r
    sDelete   = maybeToList . fmap (\ac -> ActionInfo (aId ac) True  Delete   Self "" DELETE (aInp ac) (aOut False ac) (aErr ac) (aPar ac) (aSec ac)) $ singleDelete r
    sUpdate   = map (\(s, ac) ->           ActionInfo (aId ac) False Update   Any  s  PUT    (aInp ac) (aOut False ac) (aErr ac) (aPar ac) (aSec ac)) $ singleUpdate r
    sUpdateBy = map (\(s, ac) ->           ActionInfo (aId ac) False Update   Any  s  PUT    (aInp ac) (aOut False ac) (aErr ac) (aPar ac) (aSec ac)) $ singleUpdateBy r
    sSelects  = map (\(s, ac) ->           ActionInfo (aId ac) True  Retrieve Any  s  GET    (aInp ac) (aOut False ac) (aErr ac) (aPar ac) (aSec ac)) $ singleSelects r
    sActions  = map (\(s, ac) ->           ActionInfo (aId ac) True  Modify   Any  s  POST   (aInp ac) (aOut False ac) (aErr ac) (aPar ac) (aSec ac)) $ singleActions r
    mGet      = maybeToList . fmap (\h ->  ActionInfo (hId h)  False List     Self "" GET    (hInp h)  (hOut True  h)  (hErr h)  (hPar h)  (hSec h))  $ multiGet r
    mGetBy    = map (\(s, h) ->            ActionInfo (hId h)  False List     Self s  GET    (hInp h)  (hOut True  h)  (hErr h)  (hPar h)  (hSec h))  $ multiGetBy r
    mActions  = map (\(s, ac) ->           ActionInfo (aId ac) False Modify   Any  s  POST   (aInp ac) (aOut False ac) (aErr ac) (aPar ac) (aSec ac)) $ multiActions r

  in concat [mGet, mGetBy, mActions, sGet, sGetBy, sCreate, sDelete, sUpdate, sUpdateBy, sSelects, sActions]

-- | Extract input description from handlers
handlerInputs :: Handler m a -> [DataDescription]
handlerInputs (Handler (_, _, _, inps, _, _) _ _ _) = concatMap (handlerInput (error "Don't evaluate proxy object from handlerInputs")) inps
  where handlerInput :: a -> Input a -> [DataDescription]
        handlerInput _ NoI      = []
        handlerInput _ StringI  = [defaultDescription { dataTypeDesc = "String" }]
        handlerInput _ FileI    = [defaultDescription { dataType     = File
                                                      , dataTypeDesc = "File" }]
        handlerInput d ReadI    = [defaultDescription { dataTypeDesc = describeType d }]
        handlerInput d XmlI     = [defaultDescription { dataType     = XML
                                                      , dataTypeDesc = "XML"
                                                      , dataSchema   = X.showSchema . X.getXmlSchema $ d
                                                      , dataExample  = X.showExample . X.getXmlSchema $ d
                                                      , haskellType  = typeString d
                                                      , haskellModule = modString d
                                                      }]
        handlerInput _ XmlTextI = [defaultDescription { dataType     = XML
                                                      , dataTypeDesc = "XML"
                                                      , haskellType  = "String" }]
        handlerInput d JsonI    = [defaultDescription { dataType     = JSON
                                                      , dataTypeDesc = "JSON"
                                                      , dataExample  = J.showExample . J.getJsonSchema $ d
                                                      , haskellType  = typeString d
                                                      , haskellModule = modString d
                                                      }]
        handlerInput _ RawXmlI  = [defaultDescription { dataType     = XML
                                                      , dataTypeDesc = "XML"
                                                      , haskellType  = "String"
                                                      }]

-- | Extract output description from handlers
handlerOutputs :: Bool -> Handler m a -> [DataDescription]
handlerOutputs list (Handler (_, _, _, _, outps, _) _ _ _) = concatMap (handlerOutput (error "Don't evaluate proxy object from handlerOutputs")) outps
  where handlerOutput :: a -> Output a -> [DataDescription]
        handlerOutput _ NoO      = []
        handlerOutput _ FileO    = [defaultDescription { dataType      = File
                                                       , dataTypeDesc  = "File" }]
        handlerOutput d XmlO     =
                                let schema = if list then X.getXmlListSchema d else X.getXmlSchema d
                                in [defaultDescription { dataType      = XML
                                                       , dataTypeDesc  = "XML"
                                                       , dataSchema    = X.showSchema schema
                                                       , dataExample   = X.showExample schema
                                                       , haskellType   = typeString d
                                                       , haskellModule = modString d
                                                       }]
        handlerOutput d JsonO    =
                               let schema = if list then J.getJsonListSchema d else J.getJsonSchema d
                               in  [defaultDescription { dataType      = JSON
                                                       , dataTypeDesc  = "JSON"
                                                       , dataExample   = J.showExample schema
                                                       , haskellType   = typeString d
                                                       , haskellModule = modString d
                                                       }]
        handlerOutput _ RawXmlO  = [defaultDescription { dataType     = XML
                                                       , dataTypeDesc = "XML"
                                                       , haskellType  = "String" }]
        handlerOutput _ StringO  = [defaultDescription { dataTypeDesc = "Text" }]

-- | Extract input description from handlers
handlerErrors :: Handler m a -> [DataDescription]
handlerErrors (Handler (_, _, _, _, _, ers) _ _ _) = concatMap (handleError (error "Don't evaluate proxy object from handlerInputs")) ers
  where handleError :: a -> DomainError a -> [DataDescription]
        handleError _ NoE      = []
        handleError d XmlE     = [defaultDescription { dataType      = XML
                                                     , dataTypeDesc  = "XML"
                                                     , dataSchema    = X.showSchema . X.getXmlSchema $ d
                                                     , dataExample   = X.showExample . X.getXmlSchema $ d
                                                     , haskellType   = typeString d
                                                     , haskellModule = modString d
                                                     }]
        handleError d JsonE    = [defaultDescription { dataType      = JSON
                                                     , dataTypeDesc  = "JSON"
                                                     , dataExample   = J.showExample . J.getJsonSchema $ d
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

-- | Extract whether a handler contains an identifier
handlerIdent :: Handler m a -> Maybe Ident
handlerIdent (Handler (hid, _, _, _, _, _) _ _ _) = gId hid (error "Don't evaluate proxy object from handlerIdent")
  where gId :: Action.Ident i -> i -> Maybe Ident
        gId NoId     _ = Nothing
        gId StringId _ = Just (Ident "string" "String" [])
        gId ReadId   x = Just (Ident (describeType x) (typeString x) (modString x))

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
