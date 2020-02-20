module Man where

import Data.List

data Expn = Get
          | Lit Int
          | Add Expn Expn
          | Mul Expn Expn
          | Mis Expn Expn
          | Div Expn Expn

data Expb = GetBool
          | Bl Bool
          | Bln_s Expn Expn
          | Bln_q Expn Expn
          | Bln_nq Expn Expn
          | Bln_b Expn Expn
          | Bln_sq Expn Expn
          | Bln_bq Expn Expn
          | Blb_q Expb Expb
          | Blb_nq Expb Expb

test :: Expn
test = Add (Lit 2) (Mul (Lit 6)(Lit 3))

test1 :: Expb
test1 = Bln_b (Add (Lit 2) (Mul (Lit 6)(Lit 3))) (Lit 21)

doNum :: Expn -> Int -> Int
doNum Get s = s
doNum (Lit a) s =  a
doNum (Add a b) s = (doNum a s) + (doNum b s)
doNum (Mul a b) s = (doNum a s) * (doNum b s)
doNum (Mis a b) s = (doNum a s) - (doNum b s)
doNum (Div a b) s = (doNum a s) `div` (doNum b s)

doBool :: Expb -> Int -> Bool
doBool (Bl a) s = a
doBool (Bln_s a b) s = (doNum a s) < (doNum b s)
doBool (Bln_b a b) s = (doNum a s) > (doNum b s)
doBool (Bln_q a b) s = (doNum a s) == (doNum b s)
doBool (Bln_nq a b) s = (doNum a s) /= (doNum b s)
doBool (Bln_sq a b) s = (doNum a s) <= (doNum b s)
doBool (Bln_bq a b) s = (doNum a s) >= (doNum b s)
doBool (Blb_q a b) s = (doBool a s) == (doBool b s)
doBool (Blb_nq a b) s = (doBool a s) /= (doBool b s)