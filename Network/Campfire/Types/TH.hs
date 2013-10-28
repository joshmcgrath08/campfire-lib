-- | For importing into Network.Campfire.Types for use
-- by TemplateHaskell
module Network.Campfire.Types.TH (deriveJSONOptions) where

import Data.Aeson.TH

import Data.Char(toLower, isLower)
import Data.List(intercalate, groupBy)

-- | Options for renaming fields of record types in
-- Network.Campfire.Types
deriveJSONOptions :: Options
deriveJSONOptions =
  defaultOptions { fieldLabelModifier = mixedCaseToFieldLabel }

-- goes from identifiersLikeThis to like_this
mixedCaseToFieldLabel :: String -> String
mixedCaseToFieldLabel =
  intercalate "_" .     -- concatenate words, separated by "_"
  map (map toLower) . -- lower case all characters in words
  groupBy inSameWord .  -- group words into lists
  dropWhile isLower     -- drop lower-case prefix (record selector prefix)

inSameWord :: Char -> Char -> Bool
inSameWord _ =  isLower
