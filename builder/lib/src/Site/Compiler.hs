module Site.Compiler (unsafePandocCompiler) where

import Hakyll
import qualified Site.PandocWithFilename as PWF
import Site.Filters
import Site.Utils
import Text.Pandoc (Pandoc, WriterOptions (..))
import Text.Pandoc.Walk (walk)
import Text.Pandoc.Writers.Shared (toTableOfContents)

unsafePandocCompiler = do
  ident <- getUnderlying
  numbers <- numbersEnabled ident
  let writerOptions = defaultHakyllWriterOptions {writerNumberSections = numbers}
  PWF.pandocCompilerWithTransform
    defaultHakyllReaderOptions
    writerOptions
    pandocFilter
