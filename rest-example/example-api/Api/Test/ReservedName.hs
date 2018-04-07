module Api.Test.ReservedName (resource) where

import Prelude.Compat

import Control.Monad.Reader
import Data.Text (Text)

import qualified Data.Text as Text

import Rest
import qualified Rest.Resource as R

import ApiTypes

type WithText = ReaderT Text BlogApi

resource :: Resource WithText WithText Text Void Void
resource = mkResourceId
  { R.name    = "import"
  , R.schema  = noListing $ named [("it", singleBy Text.pack)]
  , R.get     = Just get
  , R.actions = [("do", action)]
  }

get :: Handler WithText
get = mkConstHandler id $ return ()

action :: Handler WithText
action = mkConstHandler id $ return ()
