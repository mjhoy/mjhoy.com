{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Hakyll (
    hakyll
  , getResourceString
  , withItemBody
  , route
  , defaultContext
  , idRoute
  , setExtension
  , compile
  , loadAndApplyTemplate
  , templateCompiler
  , unixFilter
  , compressCss
  , pandocCompiler
  , pandocCompilerWith
  , defaultHakyllWriterOptions
  , relativizeUrls
  , copyFileCompiler
  , recentFirst
  , constField
  , loadAll
  , renderAtom
  , FeedConfiguration(..)
  , create
  , saveSnapshot
  , loadAllSnapshots
  , bodyField
  , match )

import Data.Monoid (mappend)
import Text.Pandoc (def)
import Text.Pandoc.Options (ReaderOptions(..))

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
    compile $ rawPandocCompiler
          >>= loadAndApplyTemplate "templates/default.html" defaultContext

  match "posts/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/default.html" defaultContext

  match "photo/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/photo.html" defaultContext

  match "bike/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/bike.html" defaultContext

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

  create ["rss/bike/atom.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = defaultContext `mappend` bodyField "description"

      posts <- fmap (take 10) . recentFirst =<<
        loadAllSnapshots "bike/*" "content"

      renderAtom bikeFeedConfig feedCtx posts

bikeFeedConfig :: FeedConfiguration
bikeFeedConfig= FeedConfiguration
    { feedTitle       = "Biking 2013 | mjhoy.com"
    , feedDescription = "Blogging my bike trip from Maine to Minnesota"
    , feedAuthorName  = "Michael Hoy"
    , feedAuthorEmail = "michael.john.hoy@gmail.com"
    , feedRoot        = "http://mjhoy.com"
    }

sass = getResourceString >>= withItemBody (unixFilter "sass" ["-s", "--scss"])
                         >>= return . fmap compressCss

rawPandocCompiler = pandocCompilerWith readerOptions defaultHakyllWriterOptions
  where
    readerOptions = def
      {
        readerParseRaw = True
      }
