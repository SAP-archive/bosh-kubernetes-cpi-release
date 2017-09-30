module Control.Lens.Helper.LensNames(
    sameNameRules
) where

import           Control.Lens

sameNameRules = lensRules & lensField .~ sameNameNamer

sameNameNamer = mappingNamer mapName
  where
    mapName name = [name]