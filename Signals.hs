module Signals where

import Clash.Prelude
import qualified Data.List as L

counter = s
    where
        s = register 0 (s + 1)
