module Site.Utils (isTrue, tocEnabled, numbersEnabled) where

import Hakyll

isTrue :: Maybe String -> Bool
isTrue (Just "true") = True
isTrue _ = False

tocEnabled :: Identifier -> Compiler Bool
tocEnabled ident = isTrue <$> getMetadataField ident "tocEnabled"

numbersEnabled :: Identifier -> Compiler Bool
numbersEnabled ident = isTrue <$> getMetadataField ident "numbersEnabled"
