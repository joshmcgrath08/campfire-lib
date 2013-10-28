-- | Interface to allow testing in a more modular fashion
module Network.Campfire.Test
       ( Requestable(..)
       , module Network.Campfire
       ) where

import Network.Campfire
import Network.Campfire.Request(Requestable(..))
