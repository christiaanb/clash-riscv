module Core.ALU_m where

import Clash.Prelude
import Data.Bool
import Core.Divide

data PrimaryOp
    = ADDSUB
    | SLT
    | SLTU
    | AND
    | OR
    | XOR
    | SLL
    | SR
    | MUL      -- unsigned x unsigned
    | MULH     -- signed x signed
    | MULHSU   -- signed x unsigned
    | MULHU    -- unsigned x unsigned
    | DIV      -- signed
    | DIVU     -- unsigned
    | REM      -- signed
    | REMU     -- unsigned
  deriving (Show)
  
type SecondaryOp = Bool


--operations
alu_m :: PrimaryOp -> SecondaryOp -> BitVector 32 -> BitVector 32 -> (BitVector 32, BitVector 32)
alu_m op sop x y = (addSub, alu' op sop x y)
   where
   alu' MUL    _     x y = pack (slice d31 d0  (mul (unpack x :: Unsigned 32) (unpack y :: Unsigned 32)))
   alu' MULH   _     x y = pack (slice d63 d32 (mul (unpack x :: Signed 32) (unpack y :: Signed 32)))
   alu' MULHSU _     x y = pack (slice d31 d0  ((signExtend (unpack x :: Signed 32) :: Signed 33)*(zeroExtend (unpack y :: Signed 32))))
   alu' MULHU  _     x y = pack (slice d63 d32 (mul (unpack x :: Unsigned 32) (unpack y :: Unsigned 32)))
   alu' DIV    _     x y = fst (divideS x y)
   alu' DIVU   _     x y = fst (divideU x y)
   alu' REM    _     x y = snd (divideS x y)
   alu' REMU   _     x y = snd (divideU x y)
   alu' ADDSUB _     x y = addSub
   alu' SLT    _     x y = bool 0 1 ((unpack x :: Signed 32)   < (unpack y :: Signed 32))
   alu' SLTU   _     x y = bool 0 1 ((unpack x :: Unsigned 32) < (unpack y :: Unsigned 32))
   alu' AND    _     x y = x .&. y
   alu' OR     _     x y = x .|. y
   alu' XOR    _     x y = x `xor` y
   alu' SLL    _     x y = shiftL x shiftAmt
   alu' SR     False x y = shiftR x shiftAmt
   alu' SR     True  x y = pack $ shiftR (unpack x :: Signed 32) shiftAmt
   addSub   = bool (x + y) (x - y) sop
   shiftAmt = unpack $ resize $ slice d4 d0 y

-- Unsigned division
-- for division by zero, all bits of quotient set, remainder = dividend
divideU
   :: BitVector 32                     
   -> BitVector 32                  
   -> (BitVector 32, BitVector 32)
divideU x y = (qu,ru)
   where
   (qu,ru) = if (popCount (unpack y :: Unsigned 32)) == 0 then (complement (0 :: BitVector 32),x) else (combDivide x y)

-- Signed division
-- for division by zero, quotient = -1, remainder = dividend
-- for handling overflow, quotient = -2^(xlen-1), remainder = 0
divideS
   :: BitVector 32                     
   -> BitVector 32                    
   -> (BitVector 32, BitVector 32)
divideS x y = (qs,rs)
   where
   (qs,rs) = if popCount (unpack y :: Signed 32) == 0 then (complement (0 :: BitVector 32),x) -- set values for division by zero
             else if  x ! 31 == 1 && popCount (unpack x :: Signed 32) == 1 && popCount (unpack y :: Signed 32) == 32 then (x,(0 :: BitVector 32))--set values for overflow
	     else (q_s,r_s)
   -- checking for negative numbers, converting to positive and modifying the result according to the sign
   x_u = if x ! 31 == 1 then (pack (abs (unpack x :: Signed 32)) :: BitVector 32) else x
   y_u = if y ! 31 == 1 then (pack (abs (unpack y :: Signed 32)) :: BitVector 32) else y
   (q_u, r_u) = combDivide x_u y_u
   (q_s,r_s) =
      if (x ! 31 == 0)
      then (if (y ! 31 == 0) then (q_u, r_u) else ((complement q_u) + 0b1,r_u))
      else (if (y ! 31 == 0) then ((complement q_u) + 0b1,(complement r_u)+ 0b1) else (q_u, (complement r_u)+ 0b1))
   -- sign of result of remainder operation is different from that of modulo operation
   -- remainder operation : sign of result = sign of dividend
   -- modulo operation : sign of result = sign of divisor