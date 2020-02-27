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
         | Update Var
         | Ifelse Expb Prog Prog
         | For Name Expb Type Prog
         | While Expb Prog
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
testval = [("main","t1",Val (TInt 10)),("main","t2",Val (TDouble 5.8)),("main","t3",Val (TString "123")),("main","t4",Val (TBool True))]

testFor :: Prog
testFor = [
          (Set ("i", Val (TInt 0)))
          ,
          (Set ("j", Val (TInt 0)))
          ,
          (For ("i")
               (Bli_s (Get "i") (Val (TInt 10))) 
               (TInt 1)
               [(Update ("main", "j", (Add (Get "i") (Val (TInt 1)))))] 
          )]

testWhile :: Prog
testWhile = [
            (Set ("i", Val (TInt 0)))
            ,
            (While (Bli_s (Get "i") (Val (TInt 10))) 
                   [(Update ("main", "i", (Add (Get "i") (Val (TInt 1)))))] 
            )]

testset :: Prog
testset = [(Set ("j", (Add (Get "i") (Val (TInt 1))))),(Set ("j", (Add (Get "i") (Val (TInt 1))))),(Set ("j", (Add (Get "i") (Val (TInt 1)))))]

testadd :: Type
testadd = do_operation_IntandDouble ((do_operation (Get "i") (doProg testset testval "For")), (TInt 1)) Plus

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
do_operation_IntandDouble _ a = error "Can not match tyoe Int or Double."

findVar :: Name -> [Var] -> Type
findVar a [] = error ("Can not find the name " ++ a ++ " in value list.")
findVar a ((d, e, f):xs) = if a == e then case f of Val x -> x else findVar a xs

do_operation :: Expi -> [Var] -> Type
do_operation (Get a)            s = findVar a s
do_operation (Val (TString _ )) s = error "do_operation function can not allow String type."
do_operation (Val (TBool _ ))   s = error "do_operation function can not allow Bool type."
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


updatelist :: Var -> [Var] -> [Var] -> Maybe [Var]
updatelist a         []           s = Nothing
updatelist (a, b, c) ((d,e,f):xs) s = if a == d && b == e then Just ((d, e, (Val (do_operation c s))):xs) 
                                      else case (updatelist (a, b, c) xs s) of Just x -> Just ((d,e,f):(x))
                                                                               Nothing -> Nothing

checkset :: (Cname, Name) -> [Var] -> Bool
checkset a      []              = False
checkset (a, b) ((d, e, f):xs)  = if a == d && b == e then True else checkset (a, b) xs

doCmd :: Cmd -> [Var] -> Cname -> [Var] 
doCmd (Set (a, b))    s n = if (checkset (n, a) s) then error ("Name : " ++ a ++ " in " ++ n ++ " value list. " ++ "Set command can not allow same name in sam function name.")
                            else let 
                                    ans = do_operation b s 
                                 in 
                                    ((n, a, (Val (ans))):s)
                                                        
doCmd (Ifelse a b c)  s n = if (do_Bool a s) then doProg b s n
                            else doProg c s n
doCmd (Update (a, b, c))    s n = case (updatelist (a, b, c) s s) of (Just x) -> x
                                                                     (Nothing) -> error ("Value name " ++ b ++ " not in function " ++ n ++ " value list.")
--doCmd (Operation a)  s = do_operation a s
doCmd (For a b c d)   s n = 
    if (do_Bool b s) then 
       case b of 
           (Bli_s i j) ->  let 
                             result = (doProg d s n)
                             add = do_operation_IntandDouble ((do_operation i result), c) Plus
                             newresult = doCmd (Update (n, a, (Val add))) result n
                           in 
                             doCmd (For a (Bli_s i j) c d) newresult n
           (Bli_q i j) ->  let 
                              result = (doProg d s n)
                              add = do_operation_IntandDouble ((do_operation i result), c) Plus
                              newresult = doCmd (Update (n, a, (Val add))) result n
                           in 
                              doCmd (For a (Bli_q i j) c d) newresult n
           (Bli_nq i j) -> let 
                              result = (doProg d s n)
                              add = do_operation_IntandDouble ((do_operation i result), c) Plus
                              newresult = doCmd (Update (n, a, (Val add))) result n
                           in 
                              doCmd (For a (Bli_nq i j) c d) newresult n
           (Bli_b i j) ->  let 
                              result = (doProg d s n)
                              add = do_operation_IntandDouble ((do_operation i result), c) Plus
                              newresult = doCmd (Update (n, a, (Val add))) result n
                           in 
                              doCmd (For a (Bli_b i j) c d) newresult n
           (Bli_sq i j) -> let 
                              result = (doProg d s n)
                              add = do_operation_IntandDouble ((do_operation i result), c) Plus
                              newresult = doCmd (Update (n, a, (Val add))) result  n  
                           in 
                              doCmd (For a (Bli_sq i j) c d) newresult n
           (Bli_bq i j) -> let 
                              result = (doProg d s n)
                              add = do_operation_IntandDouble ((do_operation i result), c) Plus
                              newresult = doCmd (Update (n, a, (Val add))) result n
                           in 
                              doCmd (For a (Bli_bq i j) c d) newresult n
    else s

doCmd (While b d) s n =
    if (do_Bool b s) then 
       case b of 
           (Bli_s i j) ->  let 
                             result = (doProg d s n)
                           in 
                             doCmd (While (Bli_s i j) d) result n
           (Bli_q i j) ->  let 
                              result = (doProg d s n)
                           in 
                              doCmd (While (Bli_s i j) d) result n
           (Bli_nq i j) -> let 
                              result = (doProg d s n)
                           in 
                              doCmd (While (Bli_nq i j) d) result n
           (Bli_b i j) ->  let 
                              result = (doProg d s n)
                           in 
                              doCmd (While (Bli_b i j) d) result n
           (Bli_sq i j) -> let 
                              result = (doProg d s n)
                           in 
                              doCmd (While (Bli_sq i j) d) result n
           (Bli_bq i j) -> let 
                              result = (doProg d s n)
                           in 
                              doCmd (While (Bli_bq i j) d) result n
    else s

remove_function_val :: Cname -> [Var] -> (Cname, [Var])
remove_function_val a []             = (a, [])
remove_function_val a ((b, c, d):xs) = if a == b then remove_function_val a xs
                                       else (b, ((b, c, d):xs))

doProg :: Prog -> [Var] -> Cname -> [Var]
doProg []             s n = s
doProg ((Begin a):xs) s n = doProg xs s a
doProg ((End a):xs)   s n = let (nn, ns) = (remove_function_val a s) in doProg xs ns nn
doProg (x:xs)         s n = doProg xs (doCmd x s n) n






