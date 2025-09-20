{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Site.LatexCompiler (latexCompiler) where

import Control.Exception (Exception, throw, try)
import Data.Binary (Binary (..))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Hakyll
import Hakyll.Core.Provider (resourceFilePath)
import System.Directory (copyFile, doesFileExist)
import System.Exit (ExitCode)
import System.FilePath.Posix (addExtension, takeBaseName, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (StdStream (..), createProcess, proc, spawnProcess, std_err, std_out, waitForProcess)

newtype Latex = Latex
  { tFrom :: FilePath
  }
  deriving (Generic, Binary, Eq, Ord, Show, Typeable)

latexCompiler :: Compiler (Item Latex)
latexCompiler = do
  from <- getResourceFilePath
  makeItem $ Latex from

data LatexNoFileProduced = LatexNoFileProduced
  deriving (Show)

instance Exception LatexNoFileProduced

instance Writable Latex where
  write dst (Item _ (Latex src)) = withSystemTempDirectory "hakyll-latex" $ \dir -> do
    -- NOTE(iliayar): Ignoring exit code of pdflatex, because it can be non 0, but pdf is produced
    (_, _, _, h) <-
      createProcess
        (proc "pdflatex" ["-shell-escape", "--synctex=1", "-interaction", "nonstopmode", "-output-directory", dir, src])
          { std_out = CreatePipe,
            std_err = CreatePipe
          }
    _ <- waitForProcess h
    let pdfFile = dir </> addExtension (takeBaseName src) "pdf"
    pdfFileExists <- doesFileExist pdfFile
    if pdfFileExists
      then do
        copyFile pdfFile dst
      else throw LatexNoFileProduced
