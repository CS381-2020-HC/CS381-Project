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
         | Update Var Expi
         | Ifelse Expb Prog Prog
         | For Expb Type Prog
         | Operation Expi
         deriving (Eq, Show)

-- the data of operation.
data Expi = Get Name
          | Val Type
          | Add Expi Expi
          | Mul Expi Expi
          | Mis Expi Expi
          | Div Expi Expi
          deriving (Eq, Show)

data Expm = Plus | Minus | Multiply | Divide
  deriving (Eq, Show)

data Expb = GetBool 
          | Bli Int --need rewrite
          | Bli_s Expi Expi
          | Bli_q Expi Expi
          | Bli_nq Expi Expi
          | Bli_b Expi Expi
          | Bli_sq Expi Expi
          | Bli_bq Expi Expi
          | Blb_q Expb Expb
          | Blb_nq Expb Expb
          deriving (Eq, Show)

--test :: Expi
--test = Add (Val (TInt 2)) (Mul (Val (TInt 6))(Val (TInt 3)))

--test1 :: Expb
--test1 = Bli_s (Add (Val (TInt 2)) (Mul (Val (TInt 6))(Val (TInt 3)))) (Val (TInt 20))

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

findVar :: Name -> [Var] -> Type
findVar a [] = TError
findVar a ((d, e, f):xs) = if a == e then f else findVar a xs

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
do_Bool (Bli_s a b)  s = case ((do_operation a s), (do_operation b s)) of ((TInt c), (TInt d)) -> c < d
                                                                          ((TDouble c), (TDouble d)) -> c < d
do_Bool (Bli_b a b)  s = (do_operation a s) > (do_operation b s)
do_Bool (Bli_q a b)  s = (do_operation a s) == (do_operation b s)
do_Bool (Bli_nq a b) s = (do_operation a s) /= (do_operation b s)
do_Bool (Bli_sq a b) s = (do_operation a s) <= (do_operation b s)
do_Bool (Bli_bq a b) s = (do_operation a s) >= (do_operation b s)
do_Bool (Bli a)      s = if a /= 0 then True else False
--do_Bool (Blb_q a b) s = (do_Bool a s) == (do_Bool b s)
--do_Bool (Blb_nq a b) s = (do_Bool a s) /= (do_Bool b s)

-- test2 :: Prog
-- test2 = [For (TInt 0) (Bli_s (Val (TInt 0)) (Val (TInt 10))) (TInt 1) [ Operation (Add Get (Val (TInt (-1)))) ] ]

doCmd :: Cmd -> [Var] -> [Var] 
doCmd (Set a)        s = (a:s)
doCmd (Ifelse a b c) s = if (do_Bool a s) then doProg b s 
                         else doProg c s
--doCmd (Operation a)  s = do_operation a s
doCmd (For a b c)    s = if (do_Bool a s) then case a of (Bli_s i j) -> let 
                                                                           result = (doProg c s) 
                                                                           add = do_operation_IntandDouble (i, b) Plus
                                                                        in 
                                                                           doCmd (For (Bli_s (Val add) j) b c) result
                                                         (Bli_q i j) -> let 
                                                                           result = (doProg c s) 
                                                                           add = do_operation_IntandDouble (i, b) Plus
                                                                        in 
                                                                           doCmd (For (Bli_q (Val add) j) b c) result
                                                         (Bli_nq i j) ->  let 
                                                                           result = (doProg c s) 
                                                                           add = do_operation_IntandDouble (i, b) Plus
                                                                        in 
                                                                           doCmd (For (Bli_nq (Val add) j) b c) result
                                                         (Bli_b i j) -> let 
                                                                           result = (doProg c s) 
                                                                           add = do_operation_IntandDouble (i, b) Plus
                                                                        in 
                                                                           doCmd (For (Bli_b (Val add) j) b c) result
                                                         (Bli_sq i j) -> let 
                                                                           result = (doProg c s) 
                                                                           add = do_operation_IntandDouble (i, b) Plus
                                                                        in 
                                                                           doCmd (For (Bli_sq (Val add) j) b c) result
                                                         (Bli_bq i j) -> let 
                                                                           result = (doProg c s) 
                                                                           add = do_operation_IntandDouble (i, b) Plus
                                                                        in 
                                                                           doCmd (For (Bli_bq (Val add) j) b c) result
                        else s

doProg :: Prog -> [Var] -> [Var]
doProg [] s = s
doProg (x:xs) s = doProg xs (doCmd x s)







