{-# LANGUAGE OverloadedStrings #-}

module Journal (journalRoutes) where

import Hakyll (
    route
  , defaultContext
  , setExtension
  , compile
  , loadAndApplyTemplate
  , pandocCompiler
  , copyFileCompiler
  , idRoute
  , match )

journalRoutes = do
  match "journal/**.png" $ do
    route idRoute
    compile copyFileCompiler

  match "journal/**" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/journal.html" defaultContext
