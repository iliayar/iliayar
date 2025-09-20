{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Site.LatexCompiler (latexCompiler) where

import Data.Binary (Binary (..))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Hakyll
import Hakyll.Core.Provider (resourceFilePath)
import System.Process (callProcess)
import System.Exit (ExitCode)
import Control.Exception (try)

newtype Latex = Latex
  { tFrom :: FilePath
  }
  deriving (Generic, Binary, Eq, Ord, Show, Typeable)

latexCompiler :: Compiler (Item Latex)
latexCompiler = do
  from <- getResourceFilePath
  makeItem $ Latex from

instance Writable Latex where
  write dst (Item _ (Latex src)) = do
    -- NOTE(iliayar): Ignoring exit code of pdflatex, because it can be non 0, but pdf is produced
    (_ :: Either ExitCode ()) <- try $ callProcess "pdflatex" ["-shell-escape", "--synctex=1", "-interaction", "nonstopmode", src, dst]
    return ()
