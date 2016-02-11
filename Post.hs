{-# LANGUAGE OverloadedStrings #-}

module Post (postRoutes) where

import Hakyll

postRoutes :: Rules ()
postRoutes = do

  match "posts/*.html" $ do
    route $ setExtension "html"
    compile $ getResourceBody
          >>= loadAndApplyTemplate "templates/default.html" defaultContext

  match "posts/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/default.html" defaultContext


