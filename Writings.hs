{-# LANGUAGE OverloadedStrings #-}

module Writings (writingsRoutes) where

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

writingsRoutes = do
  match "writings/**.png" $ do
    route idRoute
    compile copyFileCompiler

  match "writings/**.jpg" $ do
    route idRoute
    compile copyFileCompiler

  match "writings/**" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/writing.html" defaultContext
