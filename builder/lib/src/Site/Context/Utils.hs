module Site.Context.Utils
  ( envField,
    envFieldDef,
    yearField,
    postsCtx,
    postsCtxTake,
    postCtx,
    loadPosts,
  )
where

import Data.Functor ((<&>))
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (getCurrentTime, utctDay)
import Hakyll
import System.Environment (lookupEnv)
import Text.Printf (printf)

---------------------------------------------------------------------------------

envField :: String -> String -> Context String
envField = envField' Nothing

envFieldDef :: String -> String -> String -> Context String
envFieldDef key env defaultValue = envField' (Just defaultValue) key env

envField' :: Maybe String -> String -> String -> Context String
envField' defaultValue key env = field key $ const lookupEnvCompiler
  where
    lookupEnvCompiler :: Compiler String
    lookupEnvCompiler = do
      value <- unsafeCompiler $ lookupEnv env
      case value of
        Just value -> return value
        _ -> case defaultValue of
          Just value -> return value
          _ -> noResult $ printf "Environment variable \"%s\" is not set" env

---------------------------------------------------------------------------------

yearField :: String -> Context String
yearField key = field key $ const $ unsafeCompiler year
  where
    date :: IO (Integer, Int, Int)
    date = getCurrentTime <&> (toGregorian . utctDay)

    year :: IO String
    year = (\(y, _, _) -> show y) <$> date

---------------------------------------------------------------------------------

postCtx :: Context String
postCtx =
  defaultContext
  <> dateField "date" "%B %e, %Y"
  <> teaserField "teaser" "content"

loadPosts :: Pattern -> Maybe Int -> Compiler [Item String]
loadPosts pat takeNum = maybeTake <$> (recentFirst =<< loadAllSnapshots pat "content")
  where
    maybeTake :: [a] -> [a]
    maybeTake lst = case takeNum of
      Just takeNum -> take takeNum lst
      Nothing -> lst

postsCtx' :: Maybe Int -> String -> Pattern -> Context String
postsCtx' takeNum key pat = listField key postCtx $ loadPosts pat takeNum

postsCtx :: String -> Pattern -> Context String
postsCtx = postsCtx' Nothing

postsCtxTake :: String -> Pattern -> Int -> Context String
postsCtxTake key pat takeNum = postsCtx' (Just takeNum) key pat
