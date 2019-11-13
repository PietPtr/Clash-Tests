{-# LANGUAGE RecordWildCards #-}
module VGA where

import Clash.Prelude
import qualified Data.List as L

type LineNumber = Unsigned 11
type FrameNumber = Unsigned 10

data VGAConfig = VGAConfig
    { lineSize :: LineNumber
    , frameSize :: FrameNumber
    , horizontalPorch :: LineNumber
    , verticalPorch :: FrameNumber
    , horizontalPulse :: LineNumber
    , verticalPulse :: FrameNumber
    } deriving (Eq, Show)

config :: VGAConfig
config = VGAConfig
    { lineSize = 1056
    , horizontalPorch = 840
    , horizontalPulse = 128
    , frameSize = 628
    , verticalPorch = 601
    , verticalPulse = 4
    }

type Tick = Unsigned 0
type XCounter = Unsigned 11
type YCounter = Unsigned 10
type VGAState = (XCounter, YCounter)
type Sync = Unsigned 1
type Color = (Unsigned 8, Unsigned 8, Unsigned 8)
type Output = (Color, Sync, Sync)

vga :: VGAState -> Tick -> (VGAState, Output)
vga (xc, yc) input = (state', ((0, 0, 0), hsync, vsync))
    where
        VGAConfig{..} = config
        state' = (xc', yc'')
        yc' | xc == lineSize = yc + 1
            | otherwise = yc
        xc' | xc == lineSize = 0
            | otherwise = xc + 1
        yc'' | yc' == frameSize = 0
             | otherwise = yc + 1
        hsync | (xc > horizontalPorch) && (xc < (horizontalPorch + horizontalPulse)) = 1
              | otherwise = 0
        vsync | (yc > verticalPorch) && (yc < (verticalPorch + verticalPulse)) = 1
              | otherwise = 0

mealyVGA = mealy vga (0, 0)

topEntity
    :: Clock System
    -> Reset System
    -> Enable System
    -> Signal System (Tick)
    -> Signal System (Output)
topEntity = exposeClockResetEnable mealyVGA

horizontal = [y | (x, y, z) <- simulate @System mealyVGA [1..]]
