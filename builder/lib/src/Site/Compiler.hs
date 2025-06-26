{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Site.Compiler
  ( unsafePandocCompiler,
    copySetWritable,
    CopyFileSetWritable (..),
  ) where

import Hakyll
import qualified Site.PandocWithFilename as PWF
import Site.Filters
import Site.Utils
import Text.Pandoc (Pandoc, WriterOptions (..))
import Text.Pandoc.Walk (walk)
import Text.Pandoc.Writers.Shared (toTableOfContents)
import System.Directory (Permissions (..), copyFileWithMetadata, setPermissions, getPermissions)
import Data.Binary (Binary (..))

unsafePandocCompiler = do
  ident <- getUnderlying
  numbers <- numbersEnabled ident
  let writerOptions = defaultHakyllWriterOptions {writerNumberSections = numbers}
  PWF.pandocCompilerWithTransform
    defaultHakyllReaderOptions
    writerOptions
    pandocFilter

----------------------------------------------------------

newtype CopyFileSetWritable = CopyFileSetWritable FilePath
    deriving (Binary)

instance Writable CopyFileSetWritable where
    write dst (Item _ (CopyFileSetWritable src)) = do
        perm <- getPermissions src
        let perm' = perm { writable = True }
        copyFileWithMetadata src dst
        setPermissions dst perm'

copyFileSetWritableCompiler :: Compiler (Item CopyFileSetWritable)
copyFileSetWritableCompiler = do
    from <- getResourceFilePath
    makeItem $ CopyFileSetWritable from

copySetWritable :: Pattern -> Rules ()
copySetWritable pat =
    match pat $ do
        route idRoute
        compile copyFileSetWritableCompiler

