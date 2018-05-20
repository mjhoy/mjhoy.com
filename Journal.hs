{-# LANGUAGE OverloadedStrings #-}

module Journal (journalRoutes, journalCtx, journalPattern) where

import Hakyll
import Hakyll.Core.Metadata as M
import Data.Maybe (fromMaybe)
import Control.Lens ((<&>))

journalPattern :: Pattern
journalPattern = "journal/**.org" .||. "journal/**.html"

journalRoutes :: Rules ()
journalRoutes = do
  match "journal/**.org" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/journal.html" journalCtx
          >>= loadAndApplyTemplate "templates/layout.html" journalCtx

  match "journal/**.html" $ do
    route $ setExtension "html"
    compile $ getResourceBody
          >>= loadAndApplyTemplate "templates/journal.html" journalCtx
          >>= loadAndApplyTemplate "templates/layout.html" journalCtx

  -- source code matching: just copy it over
  match ("journal/**" .&&. complement ("journal/**.org" .||. "journal/**.html")) $ version "raw" $ do
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
