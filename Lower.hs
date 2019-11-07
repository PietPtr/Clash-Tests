module Lower where

import Clash.Prelude
import qualified Data.List as L

test :: (Unsigned 8) -> (Unsigned 4)
test a = resize a
