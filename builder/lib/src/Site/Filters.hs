module Site.Filters (pandocFilter) where

import qualified Site.Filters.Code as Code
import qualified Site.Filters.Links as Links
import Text.Pandoc (Pandoc)

-------------------------------------------------

pandocFilter :: Pandoc -> Pandoc
pandocFilter = Code.pandocFilter . Links.pandocFilter
