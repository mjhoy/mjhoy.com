{-# LANGUAGE OverloadedStrings #-}

module Journal (journalRoutes) where

import Hakyll (
    route
  , defaultContext
  , setExtension
  , compile
  , loadAndApplyTemplate
  , pandocCompiler
  , match )

journalRoutes = do
  match "journal/**" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/journal.html" defaultContext
