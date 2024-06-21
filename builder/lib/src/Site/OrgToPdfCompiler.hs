{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Site.OrgToPdfCompiler (orgToPdfCompiler) where

import Data.Binary (Binary (..))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Hakyll
import Hakyll.Core.Provider (resourceFilePath)
import System.Process (callProcess)

newtype OrgToPdf = OrgToPdf
  { otpFrom :: FilePath
  }
  deriving (Generic, Binary, Eq, Ord, Show, Typeable)

orgToPdfCompiler :: Compiler (Item OrgToPdf)
orgToPdfCompiler = do
  from <- getResourceFilePath
  makeItem $ OrgToPdf from

instance Writable OrgToPdf where
  write dst (Item _ (OrgToPdf src)) = do
    callProcess "org-to-pdf" ["--input", src, "--output", dst]
