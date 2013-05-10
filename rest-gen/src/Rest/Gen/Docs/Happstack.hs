{-# LANGUAGE FlexibleContexts #-}
module Rest.Gen.Docs.Happstack where

import Control.Monad
import Control.Monad.Trans
import Happstack.Server
import Rest.Gen.Base
import Rest.Gen.Docs.Generate
import Rest.Resource

-- | Web interface for documentation
apiDocsHandler :: (ServerMonad m, MonadPlus m, FilterMonad Response m, MonadIO m) => String -> String -> Api a -> m Response
apiDocsHandler rootURL tmpls api =
  let mkCtx v ct = DocsContext (rootURL ++ ct ++ "/") v tmpls
      serve ctx = serveDocs ctx . sortTree . noPrivate . (\(Some1 r) -> apiSubtrees r)
  in path $ \i -> withVersion i api mzero $ \v -> serve (mkCtx v i)

serveDocs :: (ServerMonad m, MonadPlus m, FilterMonad Response m, MonadIO m) => DocsContext -> ApiResource -> m Response
serveDocs ctx tree =
  msum $
    [ nullDir >> allDocsHandler ctx tree
    , docHandlers ctx tree
    ]


allDocsHandler :: (ServerMonad m, MonadPlus m, FilterMonad Response m, MonadIO m) => DocsContext -> ApiResource -> m Response
allDocsHandler ctx tree =
  do pg <- liftIO $ mkAllResources ctx tree
     setHeaderM "Content-Type" "text/html"
     return $ toResponse pg

docHandlers :: (ServerMonad m, MonadPlus m, FilterMonad Response m, MonadIO m) => DocsContext -> ApiResource -> m Response
docHandlers ctx = foldTreeChildren msum $ \it subs ->
  dir (resName it) $ msum $
       [ nullDir >> do pg <- liftIO $ mkSingleResource ctx it
                       setHeaderM "Content-Type" "text/html"
                       return $ toResponse pg
       ]
    ++ subs
