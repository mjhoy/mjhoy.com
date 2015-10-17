{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Hakyll

import Bike (bikeRoutes)
import Journal (journalRoutes)
import Writings (writingsRoutes)

main :: IO ()
main = hakyll $ do

  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "images/**/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "js/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "js/**/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "LICENSE" $ do
    route   idRoute
    compile copyFileCompiler

  match "posts/*.html" $ do
    route $ setExtension "html"
    compile $ getResourceBody
          >>= loadAndApplyTemplate "templates/default.html" defaultContext

  match "posts/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/default.html" defaultContext

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

  match "resume.html" $ do
    route idRoute
    compile copyFileCompiler

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
