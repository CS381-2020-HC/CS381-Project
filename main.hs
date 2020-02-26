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
type Var = (Cname, Name, Expi)
type LeftRight = (Type, Type)

data Cmd = Begin Cname
         | End Cname
         | Set (Name, Expi)
         | Update Name Expi
         | Ifelse Expb Prog Prog
         | For Expb Type Prog Name
         | Print Name
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


test :: Expi
test = Add (Val (TInt 2)) (Mul (Val (TInt 6))(Val (TInt 3)))

test1 :: Expb
test1 = Bli_s (Add (Val (TInt 2)) (Mul (Val (TInt 6))(Val (TInt 3)))) (Val (TInt 21))

-- test2 :: Prog
-- test2 = [For (TInt 0) (Bli_s (Val (TInt 0)) (Val (TInt 10))) (TInt 1) [ Operation (Add Get (Val (TInt (-1)))) ] ]

testval :: [Var]
testval = [("main","i",Val (TInt 0)),("main","t1",Val (TInt 10)),("main","t2",Val (TDouble 5.8)),("main","t3",Val (TString "123")),("main","t4",Val (TBool True))]

testFor :: Cmd
testFor = (For (Bli_s (Get "i") (Val (TInt 10))) 
               (TInt 1)
               [(Set ("j", (Add (Get "i") (Val (TInt 1)))))]
               ("i")
          )

--test :: Expi
--test = Add (Val (TInt 2)) (Mul (Val (TInt 6))(Val (TInt 3)))

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
findVar a ((d, e, f):xs) = if a == e then case f of Val x -> x else findVar a xs

do_operation :: Expi -> [Var] -> Type
do_operation (Get a)            s = findVar a s
do_operation (Val (TString _ )) s = TError
do_operation (Val (TBool _ ))   s = TError
do_operation (Val a)            s = a
do_operation (Add a b)          s = do_operation_IntandDouble ((do_operation a s), (do_operation b s)) Plus        
do_operation (Mul a b)          s = do_operation_IntandDouble ((do_operation a s), (do_operation b s)) Multiply       
do_operation (Mis a b)          s = do_operation_IntandDouble ((do_operation a s), (do_operation b s)) Minus       
do_operation (Div a b)          s = do_operation_IntandDouble ((do_operation a s), (do_operation b s)) Divide      


do_Bool :: Expb -> [Var] -> Bool
do_Bool (Bli_s a b)  s = case ((do_operation a s), (do_operation b s)) of ((TInt c), (TInt d)) -> c < d
                                                                          ((TDouble c), (TDouble d)) -> c < d
do_Bool (Bli_b a b)  s = case ((do_operation a s), (do_operation b s)) of ((TInt c), (TInt d)) -> c > d
                                                                          ((TDouble c), (TDouble d)) -> c > d
do_Bool (Bli_q a b)  s = case ((do_operation a s), (do_operation b s)) of ((TInt c), (TInt d)) -> c == d
                                                                          ((TDouble c), (TDouble d)) -> c == d
do_Bool (Bli_nq a b) s = case ((do_operation a s), (do_operation b s)) of ((TInt c), (TInt d)) -> c /= d
                                                                          ((TDouble c), (TDouble d)) -> c /= d
do_Bool (Bli_sq a b) s = case ((do_operation a s), (do_operation b s)) of ((TInt c), (TInt d)) -> c <= d
                                                                          ((TDouble c), (TDouble d)) -> c <= d
do_Bool (Bli_bq a b) s = case ((do_operation a s), (do_operation b s)) of ((TInt c), (TInt d)) -> c >= d
                                                                          ((TDouble c), (TDouble d)) -> c >= d
do_Bool (Bli a)      s = if a /= 0 then True else False
--do_Bool (Blb_q a b) s = (do_Bool a s) == (do_Bool b s)
--do_Bool (Blb_nq a b) s = (do_Bool a s) /= (do_Bool b s)


updatelist :: Name -> Expi -> [Var] -> [Var] -> Maybe [Var]
updatelist a b [] s = Nothing
updatelist a b ((d,e,f):xs) s = if a == e then Just ((d, e, (Val (do_operation b s))):xs) else case (updatelist a b xs s) of Just x -> Just ((d,e,f):(x))
                                                                                                                             Nothing -> Nothing

doCmd :: Cmd -> [Var] -> Cname -> [Var] 
doCmd (Set (a, b))    s n = let ans = do_operation b s in case ans of TError -> s
                                                                      _ -> ((n, a, (Val (ans))):s)
doCmd (Ifelse a b c)  s n = if (do_Bool a s) then doProg b s n
                            else doProg c s n
doCmd (Update a b)    s n = case (updatelist a b s s) of (Just x) -> x
                                                         (Nothing) -> s
--doCmd (Operation a)  s = do_operation a s
doCmd (For a b c d)   s n = 
    if (do_Bool a s) then 
       case a of 
           (Bli_s i j) ->  let 
                             result = (doProg c s n)
                             add = do_operation_IntandDouble ((do_operation i result), b) Plus
                             newresult = doCmd (Update d (Val add)) result n
                           in 
                             doCmd (For (Bli_s i j) b c d) newresult n
           (Bli_q i j) ->  let 
                              result = (doProg c s n)
                              add = do_operation_IntandDouble ((do_operation i result), b) Plus
                              newresult = doCmd (Update d (Val add)) result n
                           in 
                              doCmd (For (Bli_q i j) b c d) newresult n
           (Bli_nq i j) -> let 
                              result = (doProg c s n)
                              add = do_operation_IntandDouble ((do_operation i result), b) Plus
                              newresult = doCmd (Update d (Val add)) result n
                           in 
                              doCmd (For (Bli_nq i j) b c d) newresult n
           (Bli_b i j) ->  let 
                              result = (doProg c s n)
                              add = do_operation_IntandDouble ((do_operation i result), b) Plus
                              newresult = doCmd (Update d (Val add)) result n
                           in 
                              doCmd (For (Bli_b i j) b c d) newresult n
           (Bli_sq i j) -> let 
                              result = (doProg c s n)
                              add = do_operation_IntandDouble ((do_operation i result), b) Plus
                              newresult = doCmd (Update d (Val add)) result  n  
                           in 
                              doCmd (For (Bli_sq i j) b c d) newresult n
           (Bli_bq i j) -> let 
                              result = (doProg c s n)
                              add = do_operation_IntandDouble ((do_operation i result), b) Plus
                              newresult = doCmd (Update d (Val add)) result n
                           in 
                              doCmd (For (Bli_bq i j) b c d) newresult n
    else s

remove_function_val :: Cname -> [Var] -> (Cname, [Var])
remove_function_val a []             = (a, [])
remove_function_val a ((b, c, d):xs) = if a == b then remove_function_val a xs
                                       else (b, ((b, c, d):xs))

doProg :: Prog -> [Var] -> Cname -> [Var]
doProg []             s n = s
doProg (x:xs)         s n = doProg xs (doCmd x s n) n
doProg ((Begin a):xs) s n = doProg xs s a
doProg ((End a):xs)   s n = let (nn, ns) = (remove_function_val a s) in doProg xs ns nn






