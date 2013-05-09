{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Hakyll (
    hakyll
  , list
  , getResourceString
  , readPageCompiler
  , addDefaultFields
  , applySelf
  , route
  , idRoute
  , setExtension
  , compile
  , Compiler
  , Resource
  , applyTemplateCompiler
  , templateCompiler
  , unixFilter
  , compressCss
  , pageCompiler
  , copyFileCompiler
  , match )

import Control.Arrow ((>>>), arr)

-- Compiler for HTML pages, don't use pandoc
htmlCompiler =
  readPageCompiler >>>
  addDefaultFields >>>
  arr applySelf


main :: IO ()
main = hakyll $ do

  match "images/*" $ do
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
    compile $ htmlCompiler
          >>> applyTemplateCompiler "templates/default.html"

  match "posts/*" $ do
    route $ setExtension "html"
    compile $ pageCompiler
          >>> applyTemplateCompiler "templates/default.html"

  match "stylesheets/*" $ do
    route $ setExtension "css"
    compile sass

  match "templates/*" $ compile templateCompiler

  match (list ["index.markdown"]) $ do
    route $ setExtension "html"
    compile $ pageCompiler
          >>> applyTemplateCompiler "templates/index.html"

-- Compilers
sass :: Compiler Resource String
sass = getResourceString >>> unixFilter "sass" ["-s", "--scss"]
                         >>> arr compressCss
