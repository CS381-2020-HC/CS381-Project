module Man where

import Data.List

data Expi = Get
          | Lit Int
          | Add Expi Expi
          | Mul Expi Expi
          | Mis Expi Expi
          | Div Expi Expi

data Expb = GetBool
          | Bl Bool
          | Bli_s Expi Expi
          | Bli_q Expi Expi
          | Bli_nq Expi Expi
          | Bli_b Expi Expi
          | Bli_sq Expi Expi
          | Bli_bq Expi Expi
          | Blb_q Expb Expb
          | Blb_nq Expb Expb

test :: Expi
test = Add (Lit 2) (Mul (Lit 6)(Lit 3))

test1 :: Expb
test1 = Bli_b (Add (Lit 2) (Mul (Lit 6)(Lit 3))) (Lit 21)

doInt :: Expi -> Int -> Int
doInt Get s = s
doInt (Lit a) s =  a
doInt (Add a b) s = (doInt a s) + (doInt b s)
doInt (Mul a b) s = (doInt a s) * (doInt b s)
doInt (Mis a b) s = (doInt a s) - (doInt b s)
doInt (Div a b) s = (doInt a s) `div` (doInt b s)

doBool :: Expb -> Int -> Bool
doBool (Bl a) s = a
doBool (Bli_s a b) s = (doInt a s) < (doInt b s)
doBool (Bli_b a b) s = (doInt a s) > (doInt b s)
doBool (Bli_q a b) s = (doInt a s) == (doInt b s)
doBool (Bli_nq a b) s = (doInt a s) /= (doInt b s)
doBool (Bli_sq a b) s = (doInt a s) <= (doInt b s)
doBool (Bli_bq a b) s = (doInt a s) >= (doInt b s)
doBool (Blb_q a b) s = (doBool a s) == (doBool b s)
doBool (Blb_nq a b) s = (doBool a s) /= (doBool b s)