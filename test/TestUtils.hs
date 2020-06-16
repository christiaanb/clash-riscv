module TestUtils where

import Clash.Prelude

--Hack to make block rams output something defined on the first cycle
firstCycleDef' :: HiddenClockResetEnable dom => a -> Signal dom a -> Signal dom a
firstCycleDef' defa = mealy step False
    where
    step False _ = (True, defa)
    step True  x = (True, x)

firstCycleDef :: (HiddenClockResetEnable dom, Default a) => Signal dom a -> Signal dom a
firstCycleDef = firstCycleDef' def
