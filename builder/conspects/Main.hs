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

    flip page poDef
      $ withMetaPred "**.md"
      $ stringFieldEqOr "PUBNOTE" "html" True

    typst $ "**.typ" .&&. complement "other/**"

    latex "CT/Term2/algo/**.tex"
    latex "CT/Term2/discrete/**.tex"
    -- FIXME(iliayar): There are some other latex files

    copy ["**.png", "**.jpeg", "**.svg", "**.gif", "**.ico"]
    copy "**.pdf"
