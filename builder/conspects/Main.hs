{-# LANGUAGE OverloadedStrings #-}

import Site
import Site.Context (envFieldDef, postsCtx, postsCtxTake)
import Site.Rules (copyExternalFromEnv)

commonCtx :: Context String
commonCtx = mempty
    <> envFieldDef "assets_root" "ASSETS_ROOT" ""
    <> envField "matomo_host" "MATOMO_HOST"

poDef :: PagesOptions
poDef =
  def
    { poCommonTemplate = "other/publish/templates/common.html",
      poContext = commonCtx
    }

main :: IO ()
main = do
  conf <- configFromEnv
  hakyllWith conf $ do
    template "other/publish/templates/*"

    page "**README.org" poDef

    orgPdf
      $ withMetaPred
        ("**.org" .&&. complement "**README.org")
      $ stringFieldEqOr "PUBNOTE" "pdf" True

    flip page poDef
      $ withMetaPred
        ("**.org" .&&. complement "**README.org")
      $ stringFieldEqOr "PUBNOTE" "html" False

    typst $ "**.typ" .&&. complement "other/**"

    copy ["**.png", "**.jpeg", "**.svg", "**.gif", "**.ico"]
    copy "**.pdf"
