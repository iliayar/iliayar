{-# LANGUAGE OverloadedStrings #-}

module Site.Context.TableOfContent (tocField) where

import Hakyll
import qualified Site.PandocWithFilename as PWF
import Text.Pandoc (Pandoc, WriterOptions (..))
import Text.Pandoc.Walk (walk)
import Text.Pandoc.Writers.Shared (toTableOfContents)
import Site.Utils

tocFilter :: WriterOptions -> Pandoc -> Pandoc
tocFilter writerOptions = walk ((: []) . toTableOfContents writerOptions)

tocWriterOptions =
  defaultHakyllWriterOptions
    { writerTableOfContents = True,
      writerTOCDepth = 2
    }

tocCompiler :: Identifier -> Compiler String
tocCompiler ident = do
  toc <- tocEnabled ident
  numbers <- numbersEnabled ident
  let writerOptions = tocWriterOptions {writerNumberSections = numbers}
  if not toc
    then noResult "No toc in context because it is disabled"
    else
      itemBody
        <$> PWF.pandocCompilerWithTransform
          defaultHakyllReaderOptions
          writerOptions
          (tocFilter writerOptions)

tocField :: String -> Context String
tocField key = field key (tocCompiler . itemIdentifier)
