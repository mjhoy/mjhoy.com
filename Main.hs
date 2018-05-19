{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Hakyll
import Hakyll.Web.Template.List (recentFirst)
import Data.Time.Locale.Compat (defaultTimeLocale)
import Data.Time.Clock (utctDay, UTCTime)
import Data.Time.Calendar (toGregorian)
import Data.List (groupBy)
import Control.Lens ((<&>))

import Static (staticRoutes)
import Post (postRoutes)
import Bike (bikeRoutes)
import Journal (journalRoutes, journalCtx)
import Writings (writingsRoutes)

main :: IO ()
main = hakyll $ do

  -- images/
  -- js/
  -- LICENSE
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

  journalRoutes

  index

  bikeRoutes
  writingsRoutes

index :: Rules ()
index = create ["index.html"] $ do
  route idRoute
  compile $ do
    entries <- groupByYear =<< recentFirst =<< loadAll "journal/**.org"
    let years = traverse makeYearItem entries
    makeItem ""
      >>= loadAndApplyLayout "templates/index.html" (indexContext years)
      >>= relativizeUrls

  where
    makeYearItem (year, items) = makeItem (show year, items)
    indexContext is =
      mconcat [ listField "years" yearsContext is
              , defaultContext ]
    yearsContext =
      mconcat [ field "year" (return . fst . itemBody)
              , listFieldWith "entries" journalCtx (return . snd . itemBody) ]

loadAndApplyLayout :: Identifier -> Context String -> Item String -> Compiler (Item String)
loadAndApplyLayout temp ctx comp =
  loadAndApplyTemplate temp ctx comp >>=
  loadAndApplyTemplate "templates/layout.html" ctx

equating :: Eq b => (a -> b) -> a -> a -> Bool
equating fn x y = fn x == fn y

groupByYear :: MonadMetadata m => [Item String] -> m [(Integer, [Item String])]
groupByYear items =
  mapM (getItemUTC defaultTimeLocale . itemIdentifier) items
  <&> map getYear
  <&> flip zip items
  <&> groupBy (equating fst)
  <&> map merge

  where
    merge :: [(Integer, Item String)] -> (Integer, [Item String])
    merge ((y, x):rst) = (y, x : map snd rst)
    merge [] = (0,[])

    getYear :: UTCTime -> Integer
    getYear = fst3 . toGregorian . utctDay

    fst3 :: (a,b,c) -> a
    fst3 (a,_,_) = a
