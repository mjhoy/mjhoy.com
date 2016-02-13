{-# LANGUAGE OverloadedStrings #-}

module Journal (journalRoutes, journalCtx) where

import Hakyll

journalRoutes :: Rules ()
journalRoutes = do
  match "journal/**.org" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/journal.html" journalCtx

  -- source code matching: just copy it over
  match "journal/**" $ version "raw" $ do
    route idRoute
    compile copyFileCompiler

journalCtx :: Context String
journalCtx =
  dateField "date" "%Y/%m/%d" `mappend`
  defaultContext
