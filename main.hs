module Man where

import Data.List

type Prog = [Cmd]
data Type = TInt Int 
          | TDouble Double
          | TString String
          | TBool Bool
          | TError
          deriving (Eq, Show)

type Cname = String
type Name = String
type Var = (Cname, Name, Type)
type LeftRight = (Type, Type)

data Cmd = Begin Cname [Cmd]
         | End Cname
         | Set Var
         | Ifelse Expb Prog Prog
         | For Type Expb Type Prog
         | Operation Expi
         deriving (Eq, Show)

-- the data of operation.
data Expi = Get (Cname, Name)
          | Val Type
          | Add Expi Expi
          | Mul Expi Expi
          | Mis Expi Expi
          | Div Expi Expi
          deriving (Eq, Show)

data Expm = Plus | Minus | Multiply | Divide
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
test = Add (Val (TInt 2)) (Mul (Val (TInt 6))(Val (TInt 3)))

test1 :: Expb
test1 = Bli_s (Add (Val (TInt 2)) (Mul (Val (TInt 6))(Val (TInt 3)))) (Val (TInt 20))

do_operation_IntandDouble :: LeftRight -> Expm -> Type
do_operation_IntandDouble (TInt a, TInt b) Plus = TInt (a + b) 
do_operation_IntandDouble (TDouble a, TDouble b) Plus = TDouble (a + b)
do_operation_IntandDouble (TInt a, TDouble b) Plus = TDouble ((fromIntegral a) + b)
do_operation_IntandDouble (TDouble a, TInt b) Plus = TDouble (a + (fromIntegral b))
do_operation_IntandDouble (TInt a, TInt b) Minus = TInt (a - b) 
do_operation_IntandDouble (TDouble a, TDouble b) Minus = TDouble (a - b)
do_operation_IntandDouble (TInt a, TDouble b) Minus = TDouble ((fromIntegral a) - b)
do_operation_IntandDouble (TDouble a, TInt b) Minus = TDouble (a - (fromIntegral b))
do_operation_IntandDouble (TInt a, TInt b) Multiply = TInt (a * b) 
do_operation_IntandDouble (TDouble a, TDouble b) Multiply = TDouble (a * b)
do_operation_IntandDouble (TInt a, TDouble b) Multiply = TDouble ((fromIntegral a) * b)
do_operation_IntandDouble (TDouble a, TInt b) Multiply = TDouble (a * (fromIntegral b))
do_operation_IntandDouble (TInt a, TInt b) Divide = TInt (a `div` b) 
do_operation_IntandDouble (TDouble a, TDouble b) Divide = TDouble (a / b)
do_operation_IntandDouble (TInt a, TDouble b) Divide = TDouble ((fromIntegral a) / b)
do_operation_IntandDouble (TDouble a, TInt b) Divide = TDouble (a / (fromIntegral b))
do_operation_IntandDouble _ a = TError

findVar :: (Cname, Name) -> [Var] -> Type
findVar (a, b) [] = TError
findVar (a, b) ((d, e, f):xs) = if a == d && b == e then f else findVar (a, b) xs

do_operation :: Expi -> [Var] -> Type
do_operation (Get a)            s = findVar a s
do_operation (Val (TString _ )) s = TError
do_operation (Val (TBool _ ))   s = TError
do_operation (Val a)            s = a
do_operation (Add a b)          s = do_operation_IntandDouble ((do_operation a s), (do_operation b s)) Plus        
do_operation (Mul a b)          s = do_operation_IntandDouble ((do_operation a s), (do_operation b s)) Multiply       
do_operation (Mis a b)          s = do_operation_IntandDouble ((do_operation a s), (do_operation b s)) Minus       
do_operation (Div a b)          s = do_operation_IntandDouble ((do_operation a s), (do_operation b s)) Divide      
do_operation _                  s = TError


do_Bool :: Expb -> [Var] -> Bool
do_Bool (Bli_s a b)  s = (do_operation a s) < (do_operation b s)
do_Bool (Bli_b a b)  s = (do_operation a s) > (do_operation b s)
do_Bool (Bli_q a b)  s = (do_operation a s) == (do_operation b s)
do_Bool (Bli_nq a b) s = (do_operation a s) /= (do_operation b s)
do_Bool (Bli_sq a b) s = (do_operation a s) <= (do_operation b s)
do_Bool (Bli_bq a b) s = (do_operation a s) >= (do_operation b s)
--do_Bool (Bl a)   s = a
--do_Bool (Blb_q a b) s = (do_Bool a s) == (do_Bool b s)
--do_Bool (Blb_nq a b) s = (do_Bool a s) /= (do_Bool b s)

-- test2 :: Prog
-- test2 = [For (TInt 0) (Bli_s (Val (TInt 0)) (Val (TInt 10))) (TInt 1) [ Operation (Add Get (Val (TInt (-1)))) ] ]

doCmd :: Cmd -> [Var] -> Type 
doCmd (Set a)        s = (a:s)
doCmd (Ifelse a b c) s = if (do_Bool a s) then doProg b s 
                         else doProg c s
doCmd (Operation a)  s = do_operation a s
doCmd (For a b c d)  s = if (do_Bool b s) then case b of Bli_s i j -> let 
                                                                        result = (doProg d s) 
                                                                        add = do_operation_IntandDouble (a, c) Plus
                                                                     in 
                                                                        doCmd (For add (Bli_s (Val add) j) c d) result
                                                        Bli_q i j -> let 
                                                                        result = (doProg d s) 
                                                                        add = do_operation_IntandDouble (a, c) Plus
                                                                     in 
                                                                        doCmd (For add (Bli_q (Val add) j) c d) result
                                                        Bli_nq i j -> let 
                                                                        result = (doProg d s) 
                                                                        add = do_operation_IntandDouble (a, c) Plus
                                                                     in 
                                                                        doCmd (For add (Bli_nq (Val add) j) c d) result
                                                        Bli_b i j -> let 
                                                                        result = (doProg d s) 
                                                                        add = do_operation_IntandDouble (a, c) Plus
                                                                     in 
                                                                        doCmd (For add (Bli_b (Val add) j) c d) result
                                                        Bli_sq i j -> let 
                                                                        result = (doProg d s) 
                                                                        add = do_operation_IntandDouble (a, c) Plus
                                                                      in 
                                                                        doCmd (For add (Bli_sq (Val add) j) c d) result
                                                        Bli_bq i j -> let 
                                                                        result = (doProg d s) 
                                                                        add = do_operation_IntandDouble (a, c) Plus
                                                                      in 
                                                                        doCmd (For add (Bli_bq (Val add) j) c d) result
                        else s

doProg :: Prog -> [Var] -> Type
doProg [] s = s
doProg (x:xs) s = doProg xs (doCmd x s)







