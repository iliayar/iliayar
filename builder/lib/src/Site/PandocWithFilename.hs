--------------------------------------------------------------------------------
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | Module exporting convenient pandoc bindings
module Site.PandocWithFilename
  ( -- * The basic building blocks
    readPandoc,
    readPandocWith,
    writePandoc,
    writePandocWith,
    renderPandoc,
    renderPandocWith,
    renderPandocWithTransform,
    renderPandocWithTransformM,

    -- * Derived compilers
    pandocCompiler,
    pandocCompilerWith,
    pandocCompilerWithTransform,
    pandocCompilerWithTransformM,
  )
where

--------------------------------------------------------------------------------
import qualified Data.Text as T
--------------------------------------------------------------------------------
import Hakyll.Core.Compiler
import Hakyll.Core.Compiler.Internal
import Hakyll.Core.Item
import Hakyll.Web.Pandoc.FileType
import System.Directory
import Text.Pandoc
import Text.Pandoc.Highlighting (pygments)
import Hakyll (defaultHakyllReaderOptions, defaultHakyllWriterOptions)

--------------------------------------------------------------------------------

-- | Read a string using pandoc, with the default options
readPandoc ::
  -- | String to read with path to file
  Item (FilePath, String) ->
  -- | Resulting document
  Compiler (Item Pandoc)
readPandoc = readPandocWith defaultHakyllReaderOptions

--------------------------------------------------------------------------------

-- | Read a string using pandoc, with the supplied options
readPandocWith ::
  -- | Parser options
  ReaderOptions ->
  -- | String to read with path to file
  Item (FilePath, String) ->
  -- | Resulting document
  Compiler (Item Pandoc)
readPandocWith ropt item = do
  -- WARN: For Org's `#+include` to work
  -- But cache is not invalidating when changing included file
  res <- compilerUnsafeIO $ runIO $ traverse (reader ropt (itemFileType item) . packSource) item
  case res of
    Left err ->
      fail $
        "Hakyll.Web.Pandoc.readPandocWith: parse failed: " ++ show err
    Right item' -> return item'
  where
    reader ro t = case t of
      DocBook -> readDocBook ro
      Html -> readHtml ro
      Jupyter -> readIpynb ro
      LaTeX -> readLaTeX ro
      LiterateHaskell t' -> reader (addExt ro Ext_literate_haskell) t'
      Markdown -> readMarkdown ro
      MediaWiki -> readMediaWiki ro
      OrgMode -> readOrg ro
      Rst -> readRST ro
      Textile -> readTextile ro
      _ ->
        error $
          "Hakyll.Web.readPandocWith: I don't know how to read a file of "
            ++ "the type "
            ++ show t
            ++ " for: "
            ++ show (itemIdentifier item)

    addExt ro e = ro {readerExtensions = enableExtension e $ readerExtensions ro}

    packSource (fp, s) = [(fp, T.pack s)]

--------------------------------------------------------------------------------

-- | Write a document (as HTML) using pandoc, with the default options
writePandoc ::
  -- | Document to write
  Item Pandoc ->
  -- | Resulting HTML
  Item String
writePandoc = writePandocWith defaultHakyllWriterOptions

--------------------------------------------------------------------------------

-- | Write a document (as HTML) using pandoc, with the supplied options
writePandocWith ::
  -- | Writer options for pandoc
  WriterOptions ->
  -- | Document to write
  Item Pandoc ->
  -- | Resulting HTML
  Item String
writePandocWith wopt (Item itemi doc) =
  case runPure $ writeHtml5String wopt doc of
    Left err -> error $ "Hakyll.Web.Pandoc.writePandocWith: " ++ show err
    Right item' -> Item itemi $ T.unpack item'

--------------------------------------------------------------------------------

-- | Render the resource using pandoc
renderPandoc :: Item (FilePath, String) -> Compiler (Item String)
renderPandoc =
  renderPandocWith defaultHakyllReaderOptions defaultHakyllWriterOptions

--------------------------------------------------------------------------------

-- | Render the resource using pandoc
renderPandocWith ::
  ReaderOptions -> WriterOptions -> Item (FilePath, String) -> Compiler (Item String)
renderPandocWith ropt wopt item =
  writePandocWith wopt <$> readPandocWith ropt item

--------------------------------------------------------------------------------

-- | An extension of `renderPandocWith`, which allows you to specify a custom
-- Pandoc transformation on the input `Item`.
-- Useful if you want to do your own transformations before running
-- custom Pandoc transformations, e.g. using a `funcField` to transform raw content.
renderPandocWithTransform ::
  ReaderOptions ->
  WriterOptions ->
  (Pandoc -> Pandoc) ->
  Item (FilePath, String) ->
  Compiler (Item String)
renderPandocWithTransform ropt wopt f =
  renderPandocWithTransformM ropt wopt (return . f)

--------------------------------------------------------------------------------

-- | Similar to `renderPandocWithTransform`, but the Pandoc transformation is
-- monadic. This is useful when you want the pandoc
-- transformation to use the `Compiler` information such as routes,
-- metadata, etc. along with your own transformations beforehand.
renderPandocWithTransformM ::
  ReaderOptions ->
  WriterOptions ->
  (Pandoc -> Compiler Pandoc) ->
  Item (FilePath, String) ->
  Compiler (Item String)
renderPandocWithTransformM ropt wopt f i =
  writePandocWith wopt <$> (traverse f =<< readPandocWith ropt i)

--------------------------------------------------------------------------------

-- | Read a page render using pandoc
pandocCompiler :: Compiler (Item String)
pandocCompiler =
  pandocCompilerWith defaultHakyllReaderOptions defaultHakyllWriterOptions

--------------------------------------------------------------------------------

-- | A version of 'pandocCompiler' which allows you to specify your own pandoc
-- options
pandocCompilerWith :: ReaderOptions -> WriterOptions -> Compiler (Item String)
pandocCompilerWith ropt wopt =
  cached "Hakyll.Web.Pandoc.pandocCompilerWith" $
    pandocCompilerWithTransform ropt wopt id

--------------------------------------------------------------------------------

-- | An extension of 'pandocCompilerWith' which allows you to specify a custom
-- pandoc transformation for the content
pandocCompilerWithTransform ::
  ReaderOptions ->
  WriterOptions ->
  (Pandoc -> Pandoc) ->
  Compiler (Item String)
pandocCompilerWithTransform ropt wopt f =
  pandocCompilerWithTransformM ropt wopt (return . f)

--------------------------------------------------------------------------------

-- | Similar to 'pandocCompilerWithTransform', but the transformation
-- function is monadic. This is useful when you want the pandoc
-- transformation to use the 'Compiler' information such as routes,
-- metadata, etc
pandocCompilerWithTransformM ::
  ReaderOptions ->
  WriterOptions ->
  (Pandoc -> Compiler Pandoc) ->
  Compiler (Item String)
pandocCompilerWithTransformM ropt wopt f = do
  fp <- getResourceFilePath >>= (compilerUnsafeIO . makeAbsolute)
  item <- getResourceBody >>= withItemBody (return . (fp,))
  renderPandocWithTransformM ropt wopt f item
