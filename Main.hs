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

  match "stylesheets/main.scss" $ do
    route $ setExtension "css"
    compile sass

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

sass :: Compiler (Item String)
sass = getResourceString >>= withItemBody (unixFilter "sass" ["-s", "--scss"])
                         >>= return . fmap compressCss
