{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Site.Filters.Links (pandocFilter) where

import Data.Bifunctor (first)
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as List
import Data.Maybe
import qualified Data.Text as T
import Network.HTTP.Types.URI (decodePath, encodePathSegmentsRelative)
import Text.Pandoc (Pandoc)
import Text.Pandoc.Definition
  ( Attr,
    Inline (Image, Link),
  )
import Text.Pandoc.Walk (walk)

------------------------------------------------------

pandocFilter :: Pandoc -> Pandoc
pandocFilter = walk imageLinkFilter . walk innerLinkFilter

---------------------------------------------------------

imageExtensions :: [T.Text]
imageExtensions = [".svg"]

imageLinkFilter :: Inline -> Inline
imageLinkFilter inline@(Link attr child (T.unpack -> target, title)) =
  if isImageLink target
    then Image attr child (T.pack target, title)
    else inline
  where
    isImageLink :: String -> Bool
    isImageLink s =
      let (path, _) = decodePath $ BSC.pack s
       in case path of
            [] -> False
            path -> any (`T.isSuffixOf` last path) imageExtensions
imageLinkFilter inline = inline

--------------------------------------------------------

innerFileExtensions :: [T.Text]
innerFileExtensions = [".org", ".md"]

convertLastPathSegment :: T.Text -> [T.Text] -> [T.Text]
convertLastPathSegment ext [] = undefined
convertLastPathSegment ext [seg] = [T.dropEnd (T.length ext) seg <> ".html"]
convertLastPathSegment ext (cur : rest) = cur : convertLastPathSegment ext rest

buildLink :: BSB.Builder -> T.Text
buildLink = T.pack . BSC.unpack . BSL.toStrict . BSB.toLazyByteString

maybeInnerLink :: Inline -> Maybe Inline
maybeInnerLink inline@(Link attr child (T.unpack -> target, title)) =
  let (path, _query) = decodePath $ BSC.pack target
   in case path of
        [] -> Nothing
        path ->
          case List.find (`T.isSuffixOf` last path) innerFileExtensions of
            Just ext ->
              Just $
                Link attr child $
                  (,title) $
                    buildLink $
                      encodePathSegmentsRelative (convertLastPathSegment ext path)
            Nothing -> Nothing

innerLinkFilter :: Inline -> Inline
innerLinkFilter inline@(Link attr child (T.unpack -> target, title)) =
  fromMaybe inline (maybeInnerLink inline)
innerLinkFilter inline = inline
