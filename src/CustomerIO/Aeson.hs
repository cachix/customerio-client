-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: Apache-2.0

module CustomerIO.Aeson (defaultAesonOptions, leftoverObject, mkPair, mkObject) where

import Data.Aeson.Casing (aesonPrefix, snakeCase)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types
import Data.Maybe (catMaybes)
import GHC.Exts (fromList)

defaultAesonOptions :: Options
defaultAesonOptions = aesonPrefix snakeCase

mkPair :: ToJSON v => Key -> v -> (Key, Value)
mkPair = (.=)

mkObject :: [Maybe (Key, Value)] -> Object
mkObject = fromList . catMaybes

leftoverObject :: Object -> [Key] -> Parser (Maybe Object)
leftoverObject o keys =
  let leftover = KM.filterWithKey (\k _ -> k `notElem` keys) o
  in
  pure $
    if KM.null leftover
    then Nothing
    else Just leftover
