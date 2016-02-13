{-# LANGUAGE OverloadedStrings #-}

module Journal (journalRoutes) where

import Hakyll

journalRoutes :: Rules ()
journalRoutes = do
  match "journal/**.org" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/journal.html" defaultContext

  -- source code matching: just copy it over
  match "journal/**" $ version "raw" $ do
    route idRoute
    compile copyFileCompiler

