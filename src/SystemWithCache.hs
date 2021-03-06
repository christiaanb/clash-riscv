module System where

import Clash.Prelude
import qualified Prelude as P
import RiscV.RV32IM
import RiscV.Encode.RV32IM
import Core.Pipeline
import Prog
import Cache.ICache
import Cache.Replacement

systemWithCache :: forall dom sync gated. HiddenClockResetEnable dom => Vec (2 ^ 10) (BitVector 32) -> Signal dom Bool -> Signal dom ToDataMem
systemWithCache program instrStall = toDataMem
    where
    lines :: Vec (2 ^ 6) (Vec 16 (BitVector 32))
    lines = unconcatI program

    --The instruction memory
    fromMem :: Signal dom (Vec 16 (BitVector 32))
    fromMem = romPow2 lines ((unpack . resize) <$> memAddr)

    --The instruction cache
    (instrReady, instrData, memReq, memAddr) = iCache (SNat @ 20) (SNat @ 6) (SNat @ 4) pseudoLRUReplacement (pure True) ((pack . instructionAddress) <$> toInstructionMem) (pure True) fromMem

    --The data memory
    memReadData_3' = readNew (blockRamPow2 (repeat 0 :: Vec (2 ^ 10) (BitVector 32)))
        ((resize . readAddress)  <$> toDataMem) --read address
        (mux ((/=0) . writeStrobe <$> toDataMem) (Just <$> bundle ((resize . writeAddress) <$> toDataMem, writeData <$> toDataMem)) (pure Nothing))

    --The processor
    (toInstructionMem, toDataMem, _) = pipeline (FromInstructionMem <$> instrData <*> (not <$> instrReady)) (FromDataMem <$> memReadData_3')

{-# ANN topEntity
  (Synthesize
    { t_name   = "riscvPipeline"
    , t_inputs = [PortName "clk", PortName "rst", PortName "instrStall"]
    , t_output = PortProduct "res" [PortName "readAddress", PortName "writeAddress", PortName "writeData", PortName "writeStrobe"]
    }) #-}
topEntity :: Clock System -> Reset System -> Signal System Bool -> Signal System ToDataMem
topEntity clk rst = withClockResetEnable clk rst enableGen $ systemWithCache ($(listToVecTH (P.map encodeInstr fib)) ++ repeat 0)
