module MyCompilers (rawPandocCompiler) where

import Hakyll

import Text.Pandoc (def)
import Text.Pandoc.Options (ReaderOptions(..))

rawPandocCompiler :: Compiler (Item String)
rawPandocCompiler = pandocCompilerWith readerOptions defaultHakyllWriterOptions
  where
    readerOptions = def
      {
        readerParseRaw = True
      }
