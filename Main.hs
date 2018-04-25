{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Hakyll

import Static (staticRoutes)
import Post (postRoutes)
import Bike (bikeRoutes)
import Journal (journalRoutes)
import Writings (writingsRoutes)

main :: IO ()
main = hakyll $ do

  -- images/
  -- js/
  -- LICENSE
  -- resume.html
  staticRoutes

  -- posts/
  postRoutes

  match "photo/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/photo.html" defaultContext

  match "stylesheets/*.css" $ do
    route idRoute
    compile copyFileCompiler

  match "templates/*" $ compile templateCompiler

  match "portfolio.markdown" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/portfolio.html" defaultContext

  match "index.markdown" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/index.html" defaultContext

  bikeRoutes
  journalRoutes
  writingsRoutes
