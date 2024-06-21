{-# LANGUAGE OverloadedStrings #-}

module Site.Filters.Code (pandocFilter) where

import Data.Bifunctor (first)
import qualified Data.Text as T
import Text.Pandoc (Pandoc)
import Text.Pandoc.Definition
  ( Attr,
    Block (CodeBlock),
    Inline (Code),
  )
import Text.Pandoc.Walk (walk)

----------------------------------------------------------

pandocFilter :: Pandoc -> Pandoc
pandocFilter = walk codeBlockFilter . walk inlineCodeFilter

----------------------------------------------------------

addClass :: T.Text -> Attr -> Attr
addClass cls = first (cls :)

codeBlockFilter :: Block -> Block
codeBlockFilter (CodeBlock attr content) = CodeBlock (addClass "block-code" attr) content
codeBlockFilter block = block

inlineCodeFilter :: Inline -> Inline
inlineCodeFilter (Code attr content) = Code (addClass "inline-code" attr) content
inlineCodeFilter inline = inline

-----------------------------------------------------------
