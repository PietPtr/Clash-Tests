module MAC where

import Clash.Prelude
import qualified Data.List as L

ma acc (x, y) = acc + x * y

macT acc (x, y) = (acc', o)
    where
        acc' = ma acc (x, y)
        o = acc

mac = mealy macT 0

sine_table = (0):>(6):>(11):>(16):>(19):>(20):>(19):>(17):>
            (14):>(9):>(3):>(-3):>(-9):>(-14):>(-17):>(-20):>
            (-20):>(-19):>(-15):>(-11):>(-6):>Nil

sine start index = (idx', o)
    where
        size = length sine_table - 1
        idx' = (index + 1) `mod` size
        o = sine_table !! (index `mod` size)

sineT = mealy sine 0

counter = s
    where
        s = register 0 (s + 1)
