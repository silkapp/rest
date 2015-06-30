{-# LANGUAGE
    DataKinds
  , GADTs
  , KindSignatures
  , LambdaCase
  , NoMonomorphismRestriction
  , ScopedTypeVariables
  , TemplateHaskell
  #-}
module Rest.Gen.Base.ActionInfo
  ( Accessor
  , ActionInfo (..)
  , ActionType (..)
  , ActionTarget (..)

  , DataType (..)
  , dataTypesToAcceptHeader
  , dataTypeToAcceptHeader
  , dataTypeString

  , ResourceId
  , accessLink
  , accessors
  , chooseType

  , DataDesc (..)
  , dataType
  , haskellType
  , haskellModules

  , DataMeta (..)
  , dataTypeDesc
  , dataSchema
  , dataExample

  , DataDescription (..)
  , desc
  , meta

  , ResponseType (..)
  , responseAcceptType
  , chooseResponseType

  , isAccessor
  , listGetterActionInfo
  , mkActionDescription
  , namedActionInfo
  , resourceToAccessors
  , resourceToActionInfo
  , selectActionInfo
  , singleActionInfo
  ) where

import Prelude hiding (id, (.))

import Control.Applicative
import Control.Category
import Control.Monad
import Data.Foldable (foldMap)
import Data.Label.Derive
import Data.List
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe
import Data.Ord
import Data.Proxy
import Data.Typeable
import Safe
import qualified Data.Foldable                as F
import qualified Data.JSON.Schema             as J
import qualified Data.Label.Total             as L
import qualified Data.List.NonEmpty           as NList
import qualified Language.Haskell.Exts.Parser as H
import qualified Language.Haskell.Exts.Syntax as H

import Rest.Dictionary (Error (..), Input (..), Output (..), Param (..))
import Rest.Driver.Routing (mkListHandler, mkMultiHandler)
import Rest.Gen.Types
import Rest.Handler
import Rest.Info
import Rest.Resource hiding (description)
import Rest.Schema
import qualified Rest.Dictionary as Dict
import qualified Rest.Resource   as Rest

import Rest.Gen.Base.ActionInfo.Ident (Ident (Ident))
import Rest.Gen.Base.Link
import qualified Rest.Gen.Base.ActionInfo.Ident as Ident
import qualified Rest.Gen.Base.JSON             as J
import qualified Rest.Gen.Base.XML              as X

--------------------
-- * The types describing a resource's actions.

-- | Representation of resource
type ResourceId  = [String]

-- | Intermediate data representation of Rest structure
data RequestMethod = GET | POST | PUT | DELETE deriving (Show, Eq)

data ActionType = Retrieve | Create | Delete | DeleteMany | List | Update | UpdateMany | Modify
  deriving (Show, Eq)

data ActionTarget = Self | Any deriving (Show, Eq)

data DataType = String | XML | JSON | File | Other
  deriving (Show, Eq)

dataTypeString :: DataType -> String
dataTypeString = \case
    String -> "text"
    XML    -> "xml"
    JSON   -> "json"
    File   -> "file"
    Other  -> "text"

-- | First argument is the default accept header to use if there is no
-- output or errors, must be XML or JSON.
dataTypesToAcceptHeader :: DataType -> [DataType] -> String
dataTypesToAcceptHeader def = \case
  [] -> dataTypeToAcceptHeader def
  xs -> intercalate "," . map dataTypeToAcceptHeader . (xs ++) $
          if null (intersect xs [XML,JSON])
            then [def]
            else []

dataTypeToAcceptHeader :: DataType -> String
dataTypeToAcceptHeader = \case
  String -> "text/plain"
  XML    -> "text/xml"
  JSON   -> "application/json"
  File   -> "application/octet-stream"
  Other  -> "text/plain"

-- | Core information about the type of the input/output
data DataDesc = DataDesc
  { _dataType       :: DataType
  , _haskellType    :: H.Type
  , _haskellModules :: [H.ModuleName]
  } deriving (Show, Eq)

mkLabel ''DataDesc

-- | Documentation information about the input/output
data DataMeta = DataMeta
  { _dataTypeDesc :: String       -- ^ The name of the DataType, or a custom value if dataType is Other
  , _dataSchema   :: Maybe String -- ^ Just if dataType is XML
  , _dataExample  :: [String]     -- ^ Non empty if dataType is XML or JSON
  } deriving (Show, Eq)

mkLabel ''DataMeta

-- | Combines the core and documentation information for input/output
data DataDescription = DataDescription
  { _desc :: DataDesc
  , _meta :: DataMeta
  } deriving (Show, Eq)

mkLabel ''DataDescription

data ActionInfo = ActionInfo
  { ident        :: Maybe Ident -- Requires extra identifier in url? e.g. page/<identifier>
  , postAction   :: Bool        -- Works on identified resources? e.g. uri/<uri>/action
  , actionType   :: ActionType
  , actionTarget :: ActionTarget
  , resDir       :: String      -- Resource directory
  , method       :: RequestMethod
  , inputs       :: [DataDescription]
  , outputs      :: [DataDescription]
  , errors       :: [DataDescription]
  , params       :: [String]
  , https        :: Bool
  , link         :: Link
  } deriving (Show, Eq)

isAccessor :: ActionInfo -> Bool
isAccessor ai = actionType ai == Retrieve && actionTarget ai == Self

defaultDescription :: DataType -> String -> H.Type -> DataDescription
defaultDescription typ typeDesc htype =
  DataDescription
    { _desc = DataDesc
        { _dataType       = typ
        , _haskellType    = htype
        , _haskellModules = []
        }
    , _meta = DataMeta
        { _dataTypeDesc = typeDesc
        , _dataSchema   = Nothing
        , _dataExample  = []
        }
    }

chooseType :: NonEmpty DataDescription -> DataDescription
chooseType ls = fromMaybe (NList.head ls) $ F.find ((JSON ==) . L.get (dataType . desc)) ls

data ResponseType = ResponseType
  { errorType  :: Maybe DataDesc
  , outputType :: Maybe DataDesc
  } deriving Show

responseAcceptType :: ResponseType -> [DataType] -- TODO make non empty list Maybe (NonEmpty DataType)
responseAcceptType (ResponseType e o) = typs
  where
    typs :: [DataType]
    typs = nub $ f e ++ f o
      where
        f :: Maybe DataDesc -> [DataType]
        f = maybeToList . fmap (L.get dataType)

chooseResponseType :: ActionInfo -> ResponseType
chooseResponseType ai = case (NList.nonEmpty $ outputs ai, NList.nonEmpty $ errors ai) of
  -- No outputs or errors defined
  (Nothing, Nothing) ->
    ResponseType
      { errorType  = Nothing
      , outputType = Nothing
      }
  -- Only an error type
  (Nothing, Just e ) ->
    ResponseType
      { errorType  = Just . L.get desc $ chooseType e
      , outputType = Nothing
      }
  -- Only an output type
  (Just o , Nothing) ->
    ResponseType
      { errorType  = Nothing
      , outputType = Just . L.get desc $ chooseType o
      }
  -- Output and error
  (Just o , Just e ) -> intersection o e

  where

    intersection :: NonEmpty DataDescription -> NonEmpty DataDescription -> ResponseType
    intersection o e =
      -- Try to find a response type that can be used for both output and error.
      case intersect (f o) (f e) of
        -- If the response types are disjoint we need to specify both.
        [] ->
          ResponseType
            { errorType  = Just . L.get desc $ chooseType e
            , outputType = Just . L.get desc $ chooseType o
            }
        xs ->
          ResponseType
            { errorType  = matching xs e
            , outputType = matching xs o
            }
        where
          f = map (L.get (dataType . desc)) . NList.toList
          matching :: [DataType] -> NonEmpty DataDescription -> Maybe DataDesc
          matching dts = fmap (L.get desc) . headMay
                       -- Prioritize formats
                       . sortBy (comparing cmp)
                       -- Pick only the data types in the intersection of outputs and errors
                       . filter ((`elem` dts) . (L.get (dataType . desc)))
                       . NList.toList
          -- When we have an intersection with multiple possible
          -- types, we prefer JSON over XML, and XML over the rest.
          cmp :: DataDescription -> Int
          cmp dt = case L.get (dataType . desc) dt of
            JSON -> 0
            XML  -> 1
            _    -> 2

--------------------
-- * Traverse a resource's Schema and Handlers to create a [ActionInfo].

resourceToActionInfo :: forall m s sid mid aid. Resource m s sid mid aid -> [ActionInfo]
resourceToActionInfo r =
  case schema r of
    Schema mTopLevel step -> foldMap (topLevelActionInfo r) mTopLevel
                          ++ stepActionInfo r step
                          ++ foldMap (return . createActionInfo) (Rest.create r)
                          ++ foldMap (return . removeActionInfo accLnk) (Rest.remove r)
                          ++ map (uncurry (selectActionInfo accLnk)) (Rest.selects r)
                          ++ map (uncurry (actionActionInfo accLnk)) (Rest.actions r)
      where
        accLnk = accessLink (accessors step)

accessLink :: [Accessor] -> Link
accessLink [] = []
accessLink xs = [LAccess . map f $ xs]
  where
    f ("", x) = par x
    f (pth, x) = LAction pth : par x
    par = maybe [] (return . LParam . Ident.description)

accessors :: Step sid mid aid -> [Accessor]
accessors (Named hs) = mapMaybe (uncurry accessorsNamed) hs
  where
    accessorsNamed pth (Right (Single g)) = Just (pth, getId g)
    accessorsNamed _ _ = Nothing
    getId (Singleton _) = Nothing
    getId (By id_) = Just . idIdent $ id_
accessors (Unnamed (Single id_)) = [("", Just . idIdent $ id_)]
accessors (Unnamed (Many _)) = []

type Accessor = (String, Maybe Ident)

resourceToAccessors :: Resource m s sid mid aid -> [Accessor]
resourceToAccessors r =
  case schema r of
    Schema _ step -> accessors step

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
  ++ maybeToList (join $ multiRemoveActionInfo <$> mId <*> pure pth <*> Rest.remove r)

--------------------
-- * Smart constructors for ActionInfo.

getActionInfo :: Maybe (Id sid) -> String -> Handler m -> ActionInfo
getActionInfo mId pth = handlerActionInfo mId False Retrieve Self pth GET []

updateActionInfo :: Maybe (Id sid) -> String -> Handler m -> ActionInfo
updateActionInfo mId pth = handlerActionInfo mId False Update Any pth PUT []

multiUpdateActionInfo :: Monad m => Id sid -> String -> Handler m -> Maybe ActionInfo
multiUpdateActionInfo id_ pth h =  handlerActionInfo Nothing False UpdateMany Any pth PUT []
                               <$> mkMultiHandler id_ (const id) h

removeActionInfo :: Link -> Handler m -> ActionInfo
removeActionInfo lnk = handlerActionInfo Nothing True Delete Self "" DELETE lnk

multiRemoveActionInfo :: Monad m => Id sid -> String -> Handler m -> Maybe ActionInfo
multiRemoveActionInfo id_ pth h =  handlerActionInfo Nothing False DeleteMany Any pth DELETE []
                               <$> mkMultiHandler id_ (const id) h

listActionInfo :: Monad m => Maybe (Id mid) -> String -> ListHandler m -> Maybe ActionInfo
listActionInfo mId pth h = handlerActionInfo mId False List Self pth GET [] <$> mkListHandler h

staticActionInfo :: String -> Handler m -> ActionInfo
staticActionInfo pth = handlerActionInfo Nothing False Modify Any pth POST []

createActionInfo :: Handler m -> ActionInfo
createActionInfo = handlerActionInfo Nothing False Create Self "" POST []

selectActionInfo :: Link -> String -> Handler m -> ActionInfo
selectActionInfo lnk pth = handlerActionInfo Nothing True Retrieve Any pth GET lnk

actionActionInfo :: Link -> String -> Handler m -> ActionInfo
actionActionInfo lnk pth = handlerActionInfo Nothing True Modify Any pth POST lnk

handlerActionInfo :: Maybe (Id id)
                  -> Bool
                  -> ActionType
                  -> ActionTarget
                  -> String
                  -> RequestMethod
                  -> Link
                  -> Handler m
                  -> ActionInfo
handlerActionInfo mId postAct actType actTarget pth mth ac h = ActionInfo
  { ident        = id_
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
  , link         = makeLink
  }
  where
    id_ = idIdent <$> mId
    makeLink :: Link
    makeLink
      | postAct   = ac ++ dirPart ++ identPart
      | otherwise = dirPart ++ identPart
      where dirPart   = if pth /= ""
                        then [LAction pth]
                        else []
            identPart = maybe [] ((:[]) . LParam . Ident.description) id_


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
handlerInputs (GenHandler dict _ _) = map (handlerInput Proxy) (Dict.getDicts_ . L.get Dict.inputs $ dict)
  where
    handlerInput :: Proxy i -> Input i -> DataDescription
    handlerInput d c = case c of
      ReadI    -> L.set (haskellModules . desc) (modString d)
                $ defaultDescription Other (describe d) (toHaskellType d)
      StringI  -> defaultDescription String "String" haskellStringType
      XmlI     -> L.set (haskellModules . desc) (modString d)
                . L.set (dataSchema     . meta) (pure . X.showSchema  . X.getXmlSchema $ d)
                . L.set (dataExample    . meta) (pure . X.showExample . X.getXmlSchema $ d)
                $ defaultDescription XML "XML" (toHaskellType d)
      XmlTextI -> defaultDescription XML "XML" haskellStringType
      RawXmlI  -> defaultDescription XML "XML" haskellStringType
      JsonI    -> L.set (haskellModules . desc) (modString d)
                . L.set (dataExample    . meta) (J.showExamples . J.schema $ d)
                $ defaultDescription JSON "JSON" (toHaskellType d)
      FileI    -> defaultDescription File "File" haskellByteStringType

-- | Extract output description from handlers
handlerOutputs :: Handler m -> [DataDescription]
handlerOutputs (GenHandler dict _ _) = map (handlerOutput Proxy) (Dict.getDicts_ . L.get Dict.outputs $ dict)
  where
    handlerOutput :: Proxy a -> Output a -> DataDescription
    handlerOutput d c = case c of
      StringO -> defaultDescription String "String" haskellStringType
      XmlO    -> L.set (haskellModules . desc) (modString d)
               . L.set (dataSchema     . meta) (pure . X.showSchema  . X.getXmlSchema $ d)
               . L.set (dataExample    . meta) (pure . X.showExample . X.getXmlSchema $ d)
               $ defaultDescription XML "XML" (toHaskellType d)
      RawXmlO -> defaultDescription XML "XML" haskellStringType
      JsonO   -> L.set (haskellModules . desc) (modString d)
               . L.set (dataExample    . meta) (J.showExamples . J.schema $ d)
               $ defaultDescription JSON "JSON" (toHaskellType d)
      FileO   -> defaultDescription File "File" haskellByteStringType

-- | Extract input description from handlers
handlerErrors :: Handler m -> [DataDescription]
handlerErrors (GenHandler dict _ _) = map (handleError Proxy) (Dict.getDicts_ . L.get Dict.errors $ dict)
  where
    handleError :: Proxy a -> Error a -> DataDescription
    handleError d c = case c of
      XmlE  -> L.set (dataSchema     . meta) (pure . X.showSchema  . X.getXmlSchema $ d)
             . L.set (dataExample    . meta) (pure . X.showExample . X.getXmlSchema $ d)
             . L.set (haskellModules . desc) (modString d)
             $ defaultDescription XML "XML" (toHaskellType d)
      JsonE -> L.set (dataExample    . meta) (J.showExamples . J.schema $ d)
             . L.set (haskellModules . desc) (modString d)
             $ defaultDescription JSON "JSON" (toHaskellType d)

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

modString :: forall a. Typeable a => Proxy a -> [H.ModuleName]
modString _ = map H.ModuleName . filter (\v -> v /= "" && take 4 v /= "GHC.") . modString' . typeOf $ (undefined :: a)
  where modString' tr =
          let (tyCon, subs) = splitTyConApp tr
          in  tyConModule tyCon : concatMap modString' subs

toHaskellType :: forall a. Typeable a => Proxy a -> H.Type
toHaskellType ty =
  case H.parseType (typeString ty) of
    H.ParseOk parsedType -> parsedType
    H.ParseFailed _loc msg -> error msg

idIdent :: Id id -> Ident
idIdent (Id idnt _) = actionIdent idnt

actionIdent :: forall a. Dict.Ident a -> Ident
actionIdent Dict.StringId
  = Ident
    { Ident.description    = "string"
    , Ident.haskellType    = haskellStringType
    , Ident.haskellModules = []
    }
actionIdent Dict.ReadId
  = Ident
    { Ident.description    = describe proxy_
    , Ident.haskellType    = toHaskellType proxy_
    , Ident.haskellModules = modString proxy_
    }
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
      DeleteMany -> "Delete many " ++ targetS
      List       -> "List " ++ targetS ++ "s"
      Update     -> "Update " ++ targetS
      UpdateMany -> "Update many " ++ targetS
      Modify     -> "Modify " ++ targetS
