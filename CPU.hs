module CPU where

import Clash.Prelude
import qualified Data.List as List
import qualified Data.Bits as Bits

memory :: (Num a, Bits a) => Vec 16 a
memory =
    (ins (add', 1, 1)):>
    (ins (set', 1, 15)):>
    (ins (jmp', 0, 0)):>
    0:>
    0:>
    0:>
    0:>
    0:>
    0:>
    0:>
    0:>
    0:>
    0:>
    0:>
    0:>
    0:>Nil

nullState = (0, 0:>0:>Nil, memory)

applyr :: (Num a, Eq a) => Vec 2 a -> a -> (a -> a) -> Vec 2 a
applyr (r0:>r1:>Nil) 0 f = (f r0):>r1:>Nil
applyr (r0:>r1:>Nil) 1 f = r0:>(f r1):>Nil

cpu :: (Bits a, Enum a, Num a, Eq a) => (a, Vec 2 a, Vec 16 a) -> a -> (a, Vec 2 a, Vec 16 a)
cpu (pc, r0 :> r1 :> _, mem) tick
    -- val -> r
    | ins == 0 = (pc+1, applyr regs r (\rv -> val), mem)
    -- val + r -> r
    | ins == 1 = (pc+1, applyr regs r (\rv -> val + rv), mem)
    -- jmp to $r
    | ins == 2 = (regs !! r, regs, mem)
    -- jmp to val if r0 == 0
    | ins == 3 = (if r0 == 0 then val else pc+1, regs, mem)
    -- set mem[val] -> $r
    | ins == 4 = (pc+1, regs, replace val (regs !! r) mem)
    | otherwise = (pc, regs, mem)
    where
        regs = (r0:>r1:>Nil)
        word = mem !! pc
        val = word .&. 0b11111111
        r = (shift word (-8)) .&. 0b1
        ins = (shift word (-9)) .&. 0b111

decodeIns :: (Bits a, Num a) => a -> (a, a, a)
decodeIns word =
    ((shift word (-9)) .&. 0b111, (shift word (-8)) .&. 0b1, word .&. 255)

encodeIns :: (Bits a, Num a) => (a, a, a) -> a
encodeIns (ins, r, val) =
    (shift ins' 9) .|. (shift r' 8) .|. val'
    where
        ins' = ins .&. 0b111
        r' = r .&. 0b1
        val' = val .&. 255

ins = encodeIns

ldc' = 0
add' = 1
jmp' = 2
jm0' = 3
set' = 4

out :: a -> a
out regs = regs

mooreCPU = moore @System cpu out nullState

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (Signed 12)
  -> Signal System (Signed 12, (Vec 2 (Signed 12)), (Vec 16 (Signed 12)))
topEntity = exposeClockResetEnable mooreCPU
