{-# LANGUAGE
    EmptyDataDecls
  , FlexibleContexts
  , GADTs
  , OverloadedStrings
  , QuasiQuotes
  , TemplateHaskell
  , TypeFamilies
  #-}
module Type.PersistUser where

import Data.Text (Text)
import Database.Persist.TH

share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name Text
    password Text
    deriving Show
|]
