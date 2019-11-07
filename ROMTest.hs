module ROMTest where

import Clash.Prelude
import qualified Data.List as L

vec :: Vec 16 (Unsigned 8)
vec = 11:>16:>34:>0:>3:>95:>25:>50:>3:>9:>5:>5:>3:>9:>5:>5:>Nil

mem = rom vec

component :: () -> Unsigned 4 -> ()
component state input = 

out :: a -> a
out output = output

mooreComp = moore @System component out ()
