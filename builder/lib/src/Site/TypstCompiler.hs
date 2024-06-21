{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Site.TypstCompiler (typstCompiler) where

import Data.Binary (Binary (..))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Hakyll
import Hakyll.Core.Provider (resourceFilePath)
import System.Process (callProcess)

newtype Typst = Typst
  { tFrom :: FilePath
  }
  deriving (Generic, Binary, Eq, Ord, Show, Typeable)

typstCompiler :: Compiler (Item Typst)
typstCompiler = do
  from <- getResourceFilePath
  makeItem $ Typst from

instance Writable Typst where
  write dst (Item _ (Typst src)) = do
    callProcess "typst" ["compile", "--root", "./", src, dst]
