module Man where

import Data.List

type Prog = [Cmd]
data Cmd = Ifelse Expb Expi Expi
         deriving (Eq, Show)
type No = Either Int Double
type LeftRight = (No, No)
data Expi = Get
          | Lit No
          | Add Expi Expi
          | Mul Expi Expi
          | Mis Expi Expi
          | Div Expi Expi
          deriving (Eq, Show)

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
          deriving (Eq, Show)

test :: Expi
test = Add (Lit (Left 2)) (Mul (Lit (Left 6))(Lit (Left 3)))

test1 :: Expb
test1 = Bli_s (Add (Lit (Left 2)) (Mul (Lit (Left 6))(Lit (Left 3)))) (Lit (Left 20))

doleftright :: LeftRight -> String -> No
doleftright (Left a, Left b) "+" = Left (a + b) 
doleftright (Right a, Right b) "+" = Right (a + b)
doleftright (Left a, Right b) "+" = Right ((fromIntegral a) + b)
doleftright (Right a, Left b) "+" = Right (a + (fromIntegral b))
doleftright (Left a, Left b) "-" = Left (a - b) 
doleftright (Right a, Right b) "-" = Right (a - b)
doleftright (Left a, Right b) "-" = Right ((fromIntegral a) - b)
doleftright (Right a, Left b) "-" = Right (a - (fromIntegral b))
doleftright (Left a, Left b) "*" = Left (a * b) 
doleftright (Right a, Right b) "*" = Right (a * b)
doleftright (Left a, Right b) "*" = Right ((fromIntegral a) * b)
doleftright (Right a, Left b) "*" = Right (a * (fromIntegral b))
doleftright (Left a, Left b) "/" = Left (a `div` b) 
doleftright (Right a, Right b) "/" = Right (a / b)
doleftright (Left a, Right b) "/" = Right ((fromIntegral a) / b)
doleftright (Right a, Left b) "/" = Right (a / (fromIntegral b))

doInt :: Expi -> No -> No
doInt Get s = s
doInt (Lit a) s =  a
doInt (Add a b) s = doleftright ((doInt a s), (doInt b s)) "+"        
doInt (Mul a b) s = doleftright ((doInt a s), (doInt b s)) "*"       
doInt (Mis a b) s = doleftright ((doInt a s), (doInt b s)) "-"       
doInt (Div a b) s = doleftright ((doInt a s), (doInt b s)) "/"       

doBool :: Expb -> No -> Bool
doBool (Bl a) s = a
doBool (Bli_s a b) s = (doInt a s) < (doInt b s)
doBool (Bli_b a b) s = (doInt a s) > (doInt b s)
doBool (Bli_q a b) s = (doInt a s) == (doInt b s)
doBool (Bli_nq a b) s = (doInt a s) /= (doInt b s)
doBool (Bli_sq a b) s = (doInt a s) <= (doInt b s)
doBool (Bli_bq a b) s = (doInt a s) >= (doInt b s)
doBool (Blb_q a b) s = (doBool a s) == (doBool b s)
doBool (Blb_nq a b) s = (doBool a s) /= (doBool b s)

doCmd :: Cmd -> No -> No 
doCmd (Ifelse a b c) s = if (doBool a s) then doInt b s else doInt c s

doProg :: Prog -> No -> No
doProg [] s = s
doProg (x:xs) s = doProg xs (doCmd x s)