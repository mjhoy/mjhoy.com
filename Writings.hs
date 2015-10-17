{-# LANGUAGE OverloadedStrings #-}

module Writings (writingsRoutes) where

import Hakyll

writingsRoutes :: Rules ()
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
