{-# LANGUAGE OverloadedStrings #-}

module Bike (bikeRoutes) where

import Hakyll

bikeRoutes :: Rules ()
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
    , feedAuthorEmail = "mjh@mjhoy.com"
    , feedRoot        = "http://mjhoy.com"
    }
