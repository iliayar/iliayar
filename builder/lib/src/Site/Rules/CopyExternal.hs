{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Site.Rules.CopyExternal (copyExternalFromEnv) where

import Hakyll
import System.Environment (lookupEnv)
import System.Directory.PathWalk (pathWalk)
import System.FilePath ((</>), makeRelative)
import Control.Monad (forM_)

import Control.Monad.IO.Class (MonadIO (liftIO))

import qualified Data.ByteString.Lazy as BS

----------------------------------------------------------

newtype RulesPreprocess a = RulesPreprocess (Rules a)
    deriving (Functor, Applicative, Monad)

liftRules :: Rules a -> RulesPreprocess a
liftRules = RulesPreprocess

runRules :: RulesPreprocess a -> Rules a
runRules (RulesPreprocess rules) = rules

instance MonadIO RulesPreprocess where
    liftIO = liftRules . preprocess

------------------------------------------------------------

copyExternalFromEnv :: String -> FilePath -> Rules ()
copyExternalFromEnv fromVar toPath = do
    fromPath <- preprocess $ lookupEnv fromVar
    case fromPath of
        Just fromPath -> copyExternalFromPath fromPath toPath
        _ -> return ()

copyExternalFromPath :: FilePath -> FilePath -> Rules ()
copyExternalFromPath from to = runRules $ pathWalk from $ \dir _ files -> 
    forM_ files $ copyExternalFile' dir
    where
        copyExternalFile' dir file = 
            let fromFile = dir </> file
                toFile = to </> makeRelative from dir </> file
            in liftRules $ copyExternalFile fromFile toFile

copyExternalFile :: FilePath -> FilePath -> Rules ()
copyExternalFile from to = create [fromFilePath to] $ do
    route idRoute
    compile $ makeItem $ CopyFile from
