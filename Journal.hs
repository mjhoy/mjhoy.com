{-# LANGUAGE OverloadedStrings #-}

module Journal (journalRoutes, journalCtx) where

import Hakyll
import Hakyll.Core.Metadata as M
import Data.Maybe (fromMaybe)
import Control.Lens ((<&>))

journalRoutes :: Rules ()
journalRoutes = do
  match "journal/**.org" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/journal.html" journalCtx

  -- source code matching: just copy it over
  match ("journal/**" .&&. complement "journal/**.org") $ version "raw" $ do
    route idRoute
    compile copyFileCompiler

titleContext :: Context a
titleContext = field "title" $ \item ->
  getMetadata (itemIdentifier item)
  <&> fromMaybe "Untitled" . M.lookupString "title"

journalCtx :: Context String
journalCtx =
  titleContext `mappend`
  dateField "date" "%Y-%m-%d" `mappend`
  defaultContext
