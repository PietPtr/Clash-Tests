module Exceptions where

import Clash.Prelude
import qualified Data.List as L

func :: Unsigned 4 -> Unsigned 4
func 10 = error "wat doet u nu"
func n = n

funcT :: Unsigned 4 -> Unsigned 4 -> (Unsigned 4, Unsigned 4)
funcT state input = (state, output)
    where
        state = func input
        output = state

final = mealy funcT 0

topEntity
    ::
    Clock System
    -> Reset System
    -> Enable System
    -> Signal System (Unsigned 4)
    -> Signal System (Unsigned 4)
topEntity = exposeClockResetEnable final
