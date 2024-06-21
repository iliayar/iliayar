{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Site
  ( sass,
    copy,
    template,
    page,
    PagesOptions (..),
    def,
    rssFeed,
    orgPdf,
    typst,
    withMetaPred,
    boolFieldOr,
    stringFieldEqOr,
    configFromEnv,
    -- Hakyll re-export
    hakyll,
    hakyllWith,
    create,
    Pattern,
    (.&&.),
    (.||.),
    complement,
    Context,
    FeedConfiguration (..),
  )
where

import Control.Monad (foldM, forM_)
import Data.Default (Default, def)
import Data.String (IsString)
import Hakyll
import Hakyll.Web.Sass (sassCompiler)
import Site.Compiler (unsafePandocCompiler)
import Site.Context (tocField, yearField)
import Site.Context.Utils (loadPosts, postCtx)
import Site.OrgToPdfCompiler (orgToPdfCompiler)
import Site.TypstCompiler (typstCompiler)
import Site.Utils (isTrue)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

-------------------------------------------

-- NOTE: Why does this work with IncoherentInstances
-- Is it safe?

class PatternExt a where
  matchExt :: a -> Rules () -> Rules ()

instance (IsString a, a ~ Pattern) => PatternExt a where
  matchExt = match

instance (PatternExt a) => PatternExt [a] where
  matchExt pats rules = forM_ pats $ \pat -> matchExt pat rules

------------------------------------------------------------

data PatternWithMetaPred = PatternWithMetaPred
  { pwmdMetaPred :: Metadata -> Bool,
    pwmdPattern :: Pattern
  }

instance PatternExt PatternWithMetaPred where
  matchExt (PatternWithMetaPred {..}) = matchMetadata pwmdPattern pwmdMetaPred

withMetaPred :: Pattern -> (Metadata -> Bool) -> PatternWithMetaPred
withMetaPred pat pred =
  PatternWithMetaPred
    { pwmdPattern = pat,
      pwmdMetaPred = pred
    }

---------------------------------------------------------

boolFieldOr :: String -> Bool -> Metadata -> Bool
boolFieldOr key def meta = maybe def (isTrue . Just) $ lookupString key meta

stringFieldEqOr :: String -> String -> Bool -> Metadata -> Bool
stringFieldEqOr key value def meta = maybe def (\inner -> inner == value) $ lookupString key meta

---------------------------------------------

commonContext :: Context String
commonContext =
  defaultContext
    <> yearField "year"
    <> tocField "toc"

---------------------------------------------

data PagesOptions = PagesOptions
  { poCommonTemplate :: Identifier,
    poAdditionalTemplates :: [Identifier],
    poContext :: Context String
  }

instance Default PagesOptions where
  def =
    PagesOptions
      { poCommonTemplate = "assets/templates/common.html",
        poAdditionalTemplates = [],
        poContext = mempty
      }

-------------------------------------------

sass :: (PatternExt a) => a -> Rules ()
sass pat =
  matchExt pat $ do
    route $ setExtension "css"
    let compressCssItem = fmap compressCss
    compile $ compressCssItem <$> sassCompiler

-------------------------------------------

copy :: (PatternExt a) => a -> Rules ()
copy pat =
  matchExt pat $ do
    route idRoute
    compile copyFileCompiler

------------------------------------------

template :: (PatternExt a) => a -> Rules ()
template pat = matchExt pat $ compile templateCompiler

-------------------------------------------

page :: (PatternExt a) => a -> PagesOptions -> Rules ()
page pat po =
  matchExt pat $ do
    route $ setExtension "html"
    compile $
      let ctx = (commonContext <> poContext po)
       in unsafePandocCompiler
            >>= saveSnapshot "content"
            >>= ( \content ->
                    foldM
                      ( \content tpl ->
                          loadAndApplyTemplate tpl ctx content
                      )
                      content
                      (poAdditionalTemplates po)
                )
            >>= loadAndApplyTemplate (poCommonTemplate po) ctx

-----------------------------------------

orgPdf :: (PatternExt a) => a -> Rules ()
orgPdf pat =
  matchExt pat $ do
    route $ setExtension "pdf"
    compile orgToPdfCompiler

typst :: (PatternExt a) => a -> Rules ()
typst pat =
  matchExt pat $ do
    route $ setExtension "pdf"
    compile typstCompiler

-----------------------------------------

rssFeed :: Identifier -> Pattern -> FeedConfiguration -> Rules ()
rssFeed ident postsPat conf = create [ident] $ do
  route idRoute
  compile $ do
    let feedCtx = postCtx <> constField "description" "See post by link"
    posts <- loadPosts postsPat Nothing
    renderRss conf feedCtx posts

------------------------------------------------------

configFromEnv :: IO Configuration
configFromEnv = do
    destDir <- lookupEnv "HAKYLL_DESTINATION"    
    storeDir <- lookupEnv "HAKYLL_STORE"
    let conf = defaultConfiguration
        confDest = case destDir of
            Just dst -> conf { destinationDirectory = dst}
            Nothing -> conf
        confStore = case storeDir of
            Just store -> confDest {
                storeDirectory = store,
                tmpDirectory = store </> "tmp"
            }
            Nothing -> confDest
    return confStore
