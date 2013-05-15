module MyCompilers (rawPandocCompiler) where

import Hakyll (pandocCompilerWith, defaultHakyllWriterOptions)

import Text.Pandoc (def)
import Text.Pandoc.Options (ReaderOptions(..))

rawPandocCompiler = pandocCompilerWith readerOptions defaultHakyllWriterOptions
  where
    readerOptions = def
      {
        readerParseRaw = True
      }
