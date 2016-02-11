{-# LANGUAGE OverloadedStrings #-}

module Static (staticRoutes) where

import Hakyll

staticRoutes :: Rules ()
staticRoutes = do

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

  match "resume.html" $ do
    route idRoute
    compile copyFileCompiler
