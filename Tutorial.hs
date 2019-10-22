module Tutorial where

import Clash.Prelude
import qualified Data.List as List

ma acc (x,y) = acc + x * y
--             state  input (state, output)
macT :: Num b => b -> (b, b) -> (b, b)
macT acc (x,y) = (acc',o)
  where
    acc' = ma acc (x,y)
    o    = acc

mac = mealy @System macT 0



mooreTest :: Num b => b -> b -> b
mooreTest acc num = acc + num

f n = n

counter = moore @System mooreTest (\x -> x) 0

program :: Num a => [(a, a, a)]
program = [
    (0, 0, 0), -- do nothing
    (0, 0, 5), -- load 5 into r0
    (3, 0, 0), -- r0 + r0 -> r0
    (0, 1, 8), -- load 8 into r1
    (1, 1, 8), -- r1 + 8 -> r1
    (0, 2, 3), -- load 3 into r3
    (2, 2, 1)] -- r2 - 1 -> r2

-- program =
--     (0, 0, 0):> -- do nothing
--     (0, 0, 5):> -- load 5 into r0
--     (3, 0, 0):> -- r0 + r0 -> r0
--     (0, 1, 8):> -- load 8 into r1
--     (1, 1, 8):> -- r1 + 8 -> r1
--     (0, 2, 3):> -- load 3 into r3
--     (2, 2, 1):>Nil -- r2 - 1 -> r2

setr :: (Eq a, Num a) => (a, a, a) -> a -> a -> (a, a, a)
setr regs r val = applyr regs r (\x -> val)

getr :: (Eq a, Num a) => (a, a, a) -> a -> a
getr (r, _, _) 0 = r
getr (_, r, _) 1 = r
getr (_, _, r) 2 = r

applyr :: (Eq a, Num a) => (a, a, a) -> a -> (a -> a) -> (a, a, a)
applyr (r0, r1, r2) 0 f = (f r0, r1, r2)
applyr (r0, r1, r2) 1 f = (r0, f r1, r2)
applyr (r0, r1, r2) 2 f = (r0, r1, f r2)

cpu :: (Eq a, Num a) => (a, a, a) -> (a, a, a) -> (a, a, a)
cpu (r0, r1, r2) (ins, r, val)
    | ins == 0 = setr regs r val
    | ins == 1 = applyr regs r (\x -> x + val)
    | ins == 2 = applyr regs r (\x -> x - val)
    | ins == 3 = (r0 + (getr regs r), r1 , r2)
    where regs = (r0, r1, r2)

out :: a -> a
out regs = regs

mooreCPU = moore @System cpu out (0, 0, 0)

runCPU = List.take 7 $ simulate @System mooreCPU program

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (Signed 9, Signed 9, Signed 9)
  -> Signal System (Signed 9, Signed 9, Signed 9)
topEntity = exposeClockResetEnable mooreCPU
