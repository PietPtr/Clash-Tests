{-# LANGUAGE RecordWildCards #-}

module RISC16 where

import Clash.Prelude
import qualified Data.List as L
import qualified Data.Bits as Bits
import qualified Debug.Trace
trace = Debug.Trace.trace

type OpCode = Unsigned 3
type RegisterID = Unsigned 3
type Imm7 = Signed 7
type Imm10 = Unsigned 10

type PC = Unsigned 8
type RegisterValue = Unsigned 16
type RegisterBank = Vec 8 (RegisterValue)

data Instruction =
      ADD RegisterID RegisterID RegisterID
    | ADDI RegisterID RegisterID Imm7
    | NAND RegisterID RegisterID RegisterID
    | LUI RegisterID Imm10
    | SW RegisterID RegisterID Imm7
    | LW RegisterID RegisterID Imm7
    | BEQ RegisterID RegisterID Imm7
    | JALR RegisterID RegisterID
    deriving (Eq,Show,Read)

data Operator = Add | AddI | Nand | LoadUpperImm | Link deriving(Eq, Show)

data MemoryOperation =
      Store
    | Load
    deriving(Eq, Show)

data BranchOperation = Branch | Jump deriving(Eq, Show)

data MachineCode = MachineCode
    { aluOp     :: Operator
    , branchOp  :: Maybe BranchOperation
    , doBranch  :: Bool
    , memOp     :: Maybe MemoryOperation
    , storeRegA :: Bool
    , regA      :: Unsigned 3
    , regB      :: Unsigned 3
    , regC      :: Unsigned 3
    , imm7      :: Signed 7
    , imm10     :: Unsigned 10
    } deriving (Eq, Show)

data State = State
    { pc :: PC
    , registers :: RegisterBank
    , memory :: Vec 256 (Unsigned 16)
    } deriving (Eq, Show)



x = parse (0b0001001000000111 :: Unsigned 16)
y = parse (0b0011001000000111 :: Unsigned 16)
z = parse (0b0101001000000111 :: Unsigned 16)
a = parse (0b0111001000000111 :: Unsigned 16)
b = parse (0b1001001000000111 :: Unsigned 16)
c = parse (0b1011001000000111 :: Unsigned 16)
d = parse (0b1101001000000111 :: Unsigned 16)
e = parse (0b1111001000000111 :: Unsigned 16)

parse :: Unsigned 16 -> Instruction
parse bits = case opcode of
    0b000 -> ADD regA regB regC
    0b001 -> ADDI regA regB imm7
    0b010 -> NAND regA regB regC
    0b011 -> LUI regA imm10
    0b100 -> SW regA regB imm7
    0b101 -> LW regA regB imm7
    0b110 -> BEQ regA regB imm7
    0b111 -> JALR regA regB
    where
        opcode = (shift bits (-13))
        regA = resize (shift bits (-10)) :: Unsigned 3
        regB = resize (shift bits (-7)) :: Unsigned 3
        regC = resize bits :: Unsigned 3
        -- TODO: Better unsigned -> signed conversion?
        imm7 = fromInteger $ toInteger (resize bits :: Unsigned 7)
        imm10 = resize bits :: Unsigned 10

nop :: MachineCode
nop = MachineCode
    { aluOp     = Add
    , branchOp  = Nothing
    , doBranch  = False
    , memOp     = Nothing
    , storeRegA = True
    , regA      = 0
    , regB      = 0
    , regC      = 0
    , imm7      = 0
    , imm10     = 0
    }

decode :: Instruction -> MachineCode
decode instruction = case instruction of
    ADD     regA regB regC  -> nop {regA=regA, regB=regB, regC=regC, aluOp=Add}
    ADDI    regA regB imm7  -> nop {regA=regA, regB=regB, imm7=imm7, aluOp=AddI}
    NAND    regA regB regC  -> nop {regA=regA, regB=regB, regC=regC, aluOp=Nand}
    LUI     regA imm10      -> nop {regA=regA, imm10=imm10, aluOp=LoadUpperImm}
    SW      regA regB imm7  -> nop {regA=regA, regB=regB, imm7=imm7, memOp=Just Store, storeRegA=False}
    LW      regA regB imm7  -> nop {regA=regA, regB=regB, imm7=imm7, memOp=Just Load}
    BEQ     regA regB imm7  -> nop {regA=regA, regB=regB, imm7=imm7, branchOp=Just Branch, doBranch=(regA == regB), storeRegA=False}
    JALR    regA regB       -> nop {regA=regA, regB=regB, branchOp=Just Jump, doBranch=True, aluOp=Link}

risc16 :: State -> State
risc16 state = trace (show (machineCode, registers')) state'
    where
        State{..}       = state
        machineCode     = decode $ parse $ memory !! pc
        MachineCode{..} = machineCode

        calculatedValue = alu aluOp
            (readRegister registers regB, readRegister registers regC, imm7, imm10, pc)

        pc'             = nextPC pc branchOp doBranch imm7 (readRegister registers regB)
        registers'      = if storeRegA
            then loader registers regA calculatedValue
            else registers
        memory'         = memory

        state'          = State {pc=pc', registers=registers', memory=memory'}

nextPC :: PC -> Maybe BranchOperation -> Bool -> Imm7 -> RegisterValue -> PC
nextPC pc Nothing _ _ _ = pc + 1
nextPC pc (Just Branch) False _ _ = pc + 1
nextPC pc (Just Branch) True imm7 _ = fromInteger $ (toInteger pc) + (toInteger imm7) + 1
nextPC pc (Just Jump) _ imm7 reg = resize reg

alu :: Operator -> (RegisterValue, RegisterValue, Imm7, Imm10, PC) -> RegisterValue
alu Add (regB, regC, _, _, _) = regB + regC
alu AddI (regB, _, imm7, _, _) = fromInteger $ (toInteger regB) + (toInteger imm7)
alu Nand (regB, regC, _, _, _) = complement (regB .&. regC)
alu LoadUpperImm (_, _, _, imm10, _) = shift (resize imm10) (6)
alu Link (_, _, _, _, pc) = resize $ pc + 1

loader :: RegisterBank -> RegisterID -> RegisterValue -> RegisterBank
loader bank register value = replace register value bank

readRegister :: RegisterBank -> RegisterID -> RegisterValue
readRegister bank 0 = 0
readRegister bank regID = bank !! regID

-- mem :: MemoryOperation -> ()

state :: State
state = State
    { pc = 0
    , registers = 0:>0:>0:>0:>0:>0:>0:>0:>Nil
    , memory =
        0x24b7:>0x293f:>0x8501:>0x6400:>0x6800:>0x6c00:>0x2932:>0x249e:>
        0x0d01:>0x6400:>0x6800:>0x6c00:>0x248c:>0x290a:>0x4d01:>0x0000:>
        0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>
        0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>
        0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>
        0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>
        0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>
        0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>

        0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>
        0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>
        0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>
        0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>
        0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>
        0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>
        0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>
        0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>

        0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>
        0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>
        0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>
        0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>
        0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>
        0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>
        0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>
        0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>

        0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>
        0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>
        0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>
        0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>
        0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>
        0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>
        0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>
        0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>0x0000:>Nil
    }

loop :: State -> Int -> [State]
loop state 0 = []
loop state n = state : (loop (risc16 state) (n - 1))

epl n = mapM_ print $ loop state n
