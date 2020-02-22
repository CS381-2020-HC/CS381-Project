module Man where

import Data.List

type Prog = [Cmd]
type No = Either Int Double
type LeftRight = (No, No)

data Cmd = Ifelse Expb Prog Prog
         | For No Expb No Prog
         | Operation Expi
         deriving (Eq, Show)

data Expi = Get
          | Lit No
          | Add Expi Expi
          | Mul Expi Expi
          | Mis Expi Expi
          | Div Expi Expi
          deriving (Eq, Show)

data Expm = Plus | Minus | Multiply | Divide

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

do_operation_IntandDouble :: LeftRight -> Expm -> No
do_operation_IntandDouble (Left a, Left b) Plus = Left (a + b) 
do_operation_IntandDouble (Right a, Right b) Plus = Right (a + b)
do_operation_IntandDouble (Left a, Right b) Plus = Right ((fromIntegral a) + b)
do_operation_IntandDouble (Right a, Left b) Plus = Right (a + (fromIntegral b))
do_operation_IntandDouble (Left a, Left b) Minus = Left (a - b) 
do_operation_IntandDouble (Right a, Right b) Minus = Right (a - b)
do_operation_IntandDouble (Left a, Right b) Minus = Right ((fromIntegral a) - b)
do_operation_IntandDouble (Right a, Left b) Minus = Right (a - (fromIntegral b))
do_operation_IntandDouble (Left a, Left b) Multiply = Left (a * b) 
do_operation_IntandDouble (Right a, Right b) Multiply = Right (a * b)
do_operation_IntandDouble (Left a, Right b) Multiply = Right ((fromIntegral a) * b)
do_operation_IntandDouble (Right a, Left b) Multiply = Right (a * (fromIntegral b))
do_operation_IntandDouble (Left a, Left b) Divide = Left (a `div` b) 
do_operation_IntandDouble (Right a, Right b) Divide = Right (a / b)
do_operation_IntandDouble (Left a, Right b) Divide = Right ((fromIntegral a) / b)
do_operation_IntandDouble (Right a, Left b) Divide = Right (a / (fromIntegral b))

do_operation :: Expi -> No -> No
do_operation Get s = s
do_operation (Lit a) s =  a
do_operation (Add a b) s = do_operation_IntandDouble ((do_operation a s), (do_operation b s)) Plus        
do_operation (Mul a b) s = do_operation_IntandDouble ((do_operation a s), (do_operation b s)) Multiply       
do_operation (Mis a b) s = do_operation_IntandDouble ((do_operation a s), (do_operation b s)) Minus       
do_operation (Div a b) s = do_operation_IntandDouble ((do_operation a s), (do_operation b s)) Divide      

do_Bool :: Expb -> No -> Bool
--do_Bool (Bl a) s = a
do_Bool (Bli_s a b) s = (do_operation a s) < (do_operation b s)
do_Bool (Bli_b a b) s = (do_operation a s) > (do_operation b s)
do_Bool (Bli_q a b) s = (do_operation a s) == (do_operation b s)
do_Bool (Bli_nq a b) s = (do_operation a s) /= (do_operation b s)
do_Bool (Bli_sq a b) s = (do_operation a s) <= (do_operation b s)
do_Bool (Bli_bq a b) s = (do_operation a s) >= (do_operation b s)
--do_Bool (Blb_q a b) s = (do_Bool a s) == (do_Bool b s)
--do_Bool (Blb_nq a b) s = (do_Bool a s) /= (do_Bool b s)

test2 :: Prog
test2 = [For (Left 0) (Bli_s (Lit (Left 0)) (Lit (Left 10))) (Left 1) [ Operation (Add Get (Lit (Left (-1)))) ] ]

doCmd :: Cmd -> No -> No 
doCmd (Ifelse a b c) s = if (do_Bool a s) then doProg b s 
                         else doProg c s
doCmd (Operation a) s = do_operation a s
doCmd (For a b c d) s = if (do_Bool b s) then case b of Bli_s i j -> let 
                                                                        result = (doProg d s) 
                                                                        add = do_operation_IntandDouble (a, c) Plus
                                                                     in 
                                                                        doCmd (For add (Bli_s (Lit add) j) c d) result
                                                        Bli_q i j -> let 
                                                                        result = (doProg d s) 
                                                                        add = do_operation_IntandDouble (a, c) Plus
                                                                     in 
                                                                        doCmd (For add (Bli_q (Lit add) j) c d) result
                                                        Bli_nq i j -> let 
                                                                        result = (doProg d s) 
                                                                        add = do_operation_IntandDouble (a, c) Plus
                                                                     in 
                                                                        doCmd (For add (Bli_nq (Lit add) j) c d) result
                                                        Bli_b i j -> let 
                                                                        result = (doProg d s) 
                                                                        add = do_operation_IntandDouble (a, c) Plus
                                                                     in 
                                                                        doCmd (For add (Bli_b (Lit add) j) c d) result
                                                        Bli_sq i j -> let 
                                                                        result = (doProg d s) 
                                                                        add = do_operation_IntandDouble (a, c) Plus
                                                                      in 
                                                                        doCmd (For add (Bli_sq (Lit add) j) c d) result
                                                        Bli_bq i j -> let 
                                                                        result = (doProg d s) 
                                                                        add = do_operation_IntandDouble (a, c) Plus
                                                                      in 
                                                                        doCmd (For add (Bli_bq (Lit add) j) c d) result
                        else s

doProg :: Prog -> No -> No
doProg [] s = s
doProg (x:xs) s = doProg xs (doCmd x s)