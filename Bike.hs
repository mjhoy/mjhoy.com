{-# LANGUAGE OverloadedStrings #-}

module Bike (bikeRoutes) where

import Hakyll (
    route
  , defaultContext
  , idRoute
  , setExtension
  , compile
  , loadAndApplyTemplate
  , pandocCompiler
  , recentFirst
  , renderAtom
  , FeedConfiguration(..)
  , create
  , saveSnapshot
  , loadAllSnapshots
  , bodyField
  , match )

import Data.Monoid (mappend)

bikeRoutes = do
  match "bike/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/bike.html" defaultContext

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
