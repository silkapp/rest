{-# LANGUAGE
    EmptyDataDecls
  , FlexibleContexts
  , GADTs
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , OverloadedStrings
  , QuasiQuotes
  , TemplateHaskell
  , TypeFamilies
  #-}
module Type.PersistUser where

import Data.Text (Text)
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name Text
    password Text
    deriving Show
|]
