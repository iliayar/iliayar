{-# LANGUAGE OverloadedStrings #-}

import Site
import Site.Context (envFieldDef, postsCtxTake, postsCtx)
import Site.Rules (copyExternalFromEnv)

rssFeedConfiguration :: FeedConfiguration
rssFeedConfiguration = FeedConfiguration
    { feedTitle       = "iliayar's blog"
    , feedDescription = "Blog feed"
    , feedAuthorName  = "Ilya Yaroshevskiy"
    , feedAuthorEmail = "iliayar3@gmail.com"
    , feedRoot        = "http://iliay.ar"
    }

commonCtx :: Context String
commonCtx = mempty
    <> envField "matomo_host" "MATOMO_HOST"

posts :: Pattern
posts = "blog/*/index*"

main :: IO ()
main = do
  conf <- configFromEnv
  hakyllWith conf $ do
    sass "assets/css/*.scss"
    copy "assets/js/*.js"
    copyExternalFromEnv "THIRDPARTY_PATH" "assets/thirdparty"

    template "assets/templates/*"

    page posts $
      def
        { poContext = commonCtx
        }

    page "index.org" $
      def
        { poContext = commonCtx <> postsCtxTake "posts" posts 5
        , poAdditionalTemplates = ["assets/templates/index-with-posts.html"]
        }

    page "blog/index.org" $
      def
        { poContext = commonCtx <> postsCtx "posts" posts
        , poAdditionalTemplates = ["assets/templates/blog-page-with-posts.html"]
        }

    page ["**.org", "**.md"] $
      def
        { poContext = commonCtx
        }

    copy ["**.png", "**.jpeg", "**.svg", "**.gif", "**.ico"]
    copy "**.pdf"

    rssFeed "rss.xml" posts rssFeedConfiguration
