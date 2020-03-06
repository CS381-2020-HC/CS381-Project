module Man where
-- At First we use Main for module name, but it will get wrong so we change to this name.

import Data.List
import Data.Data
import Data.Function

-- Prog is the Value of list Cmd.
type Prog = [Cmd]

-- Define a Value data for the Int, Double, String, Bool.
-- We put the Float Value and Double Value together.
data Value = TInt Int         -- Int
           | TDouble Double   -- Float and Double
           | TString String   -- String
           | TBool Bool       -- Bool
           | TError           -- not designe yet.
           deriving (Eq, Show)

-- Fname is the Function Name.
type Fname = String

-- Name is the variable name.
type Name = String

-- Define the value list base Value, and it include Function name, value name, and the Expr Value to do the operation.
type Var = (Fname, Name, Expr)

-- This Value will change the name in the fucture.
-- This Value just for know the two value Value in do_operation_IntandDouble.
type LeftRight = (Value, Value)  -- will rewrite the Value name

type EnvironmentData = ([Var], [String], [(Fname, Prog)])

-- Define the Cmd data.
-- Begin and End is to call new function and End the function.
-- Set and Update is do the same thing but set is for define a value in value list, and
-- Update is to change the value list value.
-- Ifelse have a condiction if True do the first Prog else do the second Prog.
-- For is to do the the same thing multiple time, but you need to define value it used before.
-- while is do the thing until the condiction is false.
-- Print is to show something.
data Cmd = Begin Fname
         | End Fname
         | Set (Name, Expr)
         | Update Var
         | Ifelse Expb Prog Prog
         | For Name Expb Value Prog
         | While Expb Prog
         | Print Expr
         | Return Expr
         | SetFunction Fname [Var] Prog
         | CallFunction Fname [Var]
--         | Operation Expr -- could remove.
         deriving (Eq, Show)

-- the data of operation.
-- Get is to get the value call Name in value list.
-- Val is the value which you can do the operation.
data Expr = Get Name
          | Val Value
          | Add Expr Expr    -- +
          | Mul Expr Expr    -- *
          | Mis Expr Expr    -- -
          | Div Expr Expr    -- /
          | Mod Expr Expr    -- %
          deriving (Eq, Show)

-- We give the name for do the operation for +,-,*,/ by create data Value.
data Oper = Plus | Minus | Multiply | Divide | Remainder
  deriving (Eq, Show)

-- Expb is the condiction.
data Expb = GetBool 
          | Bli Int           -- Will rewrite
          | Blv_s Expr Expr   -- Left Smaller then Right.
          | Blv_q Expr Expr   -- Left Equal with Right.
          | Blv_nq Expr Expr  -- Left not Equal with Right.
          | Blv_b Expr Expr   -- Left Bigger then Right.
          | Blv_sq Expr Expr  -- Left Smaller and Equal then Right.
          | Blv_bq Expr Expr  -- Left Bigger and Equal then Right.
          | Blb_q Expb Expb   -- Will Remove
          | Blb_nq Expb Expb  -- Will Remove
          deriving (Eq, Show)



data OutPut = Env [Var]
            | Prt [String]

testoperation :: Expr
testoperation = Add (Val (TInt 2)) (Mul (Val (TInt 6))(Val (TInt 3)))

testcondiction :: Expb
testcondiction = Blv_s (Add (Val (TInt 2)) (Mul (Val (TInt 6))(Val (TInt 3)))) (Val (TInt 21))

-- test2 :: Prog
-- test2 = [For (TInt 0) (Blv_s (Val (TInt 0)) (Val (TInt 10))) (TInt 1) [ Operation (Add Get (Val (TInt (-1)))) ] ]

testval :: EnvironmentData
testval = ([("main","t1",Val (TInt 10)),("main","t2",Val (TDouble 5.8)),("main","t3",Val (TString "123")),("main","t4",Val (TBool True))], [], [])

testFor :: Prog
testFor = [
          (Set ("i", Val (TInt 0)))
          ,
          (Set ("j", Val (TInt 0)))
          ,
          (For ("i")
               (Blv_s (Get "i") (Val (TInt 10))) 
               (TInt 1)
               [(Update ("main", "j", (Add (Get "i") (Val (TInt 1)))))] 
          )]

testWhile :: Prog
testWhile = [
            (Set ("i", Val (TInt 0)))
            ,
            (While (Blv_s (Get "i") (Val (TInt 10))) 
                   [(Update ("main", "i", (Add (Get "i") (Val (TInt 1)))))] 
            )]

testset :: Prog
testset = [(Set ("j", (Add (Get "i") (Val (TInt 1))))),(Set ("j", (Add (Get "i") (Val (TInt 1))))),(Set ("j", (Add (Get "i") (Val (TInt 1)))))]

testadd :: Value
testadd = let (v, s, f) = (doProg testset testval "For") in do_operation_IntandDouble ((do_operation (Get "i") v), (TInt 1)) Plus

testIfElse :: Prog
testIfElse = [
             Set ("i", Val (TInt 0)),
             Ifelse (Blv_s (Get "i")
                    (Mis (Val (TInt 200)) (Val (TInt 100))))
                    [
                        (Update ("main", "i", (Add (Get "i") (Val (TInt 1)))))
                    ]
                    [
                        (Update ("main", "i", (Add (Get "i") (Val (TInt 2)))))
                    ]
             ]
             
-- testall () {
--    if (i < (200-100)){
--       int j = 0;
--       for(int i = 0; i < 10; i = i + 2){
--          j = i + 1;
--       }
--    } else {
--       while (i < 10){
--          i++ ;
--       }
--       j = i;
--    }
-- }

testall :: Prog
testall = [
          Begin "testall",
          Set ("i", Val (TInt 0)),
          Begin "ifelse",
          Ifelse (Blv_s (Get "i") 
                 (Mis (Val (TInt 200)) (Val (TInt 100)))) 
                 [
                     Set ("j", Val (TInt 0)),
                     Begin "for",
                     For ("i") 
                         (Blv_s (Get "i") (Val (TInt 10))) 
                         (TInt 2) 
                         [
                           (Update ("ifelse", "j", (Add (Get "i") (Val (TInt 1)))))
                         ],
                     End "for"
                 ] 
                 [
                     Begin "while",
                     While (Blv_s (Get "i") (Val (TInt 10))) 
                           [(Update ("ifelse", "i", (Add (Get "i") (Val (TInt 1)))))],
                     Set ("j", Get "i"),
                     End "while"
                 ],
          End "ifelse",
          End "testall"
          ]

-- this is for testing the 1071 and 462 euclidean algorithm
-- the answer is in the value list name m.
euclidean_algorithm :: Prog
euclidean_algorithm = [
                        SetFunction "gcd" 
                        [("gcd", "m", Val (TInt 0)),("gcd", "n", Val (TInt 0))]
                        [Begin "gcd",
                        Set ("t", Val (TInt 1)),
                        While (Blv_nq (Get "t") (Val (TInt 0))) 
                              [
                                 Update ("gcd", "t", Mod (Get "m") (Get "n")),
                                 Update ("gcd", "m", Get "n"),
                                 Update ("gcd", "n", Get "t")
                              ],
                        Return (Get "m")],
                        CallFunction "gcd" [("gcd", "m", Val (TInt 1071)),("gcd", "n", Val (TInt 462))],    
                        Set ("x", Get "return"),
                        Print (Get "x")
                      ]

--test :: Expr
--test = Add (Val (TInt 2)) (Mul (Val (TInt 6))(Val (TInt 3)))

do_operation_IntandDouble :: LeftRight -> Oper -> Value
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
do_operation_IntandDouble (TInt a, TInt b) Remainder = TInt (a `mod` b) 
do_operation_IntandDouble (TDouble a, TDouble b) Remainder = error "Remainder only can input two Int."
do_operation_IntandDouble (TInt a, TDouble b) Remainder = error "Remainder only can input two Int."
do_operation_IntandDouble (TDouble a, TInt b) Remainder = error "Remainder only can input two Int."
do_operation_IntandDouble _ a = error "Can not match Value Int or Double."

findVar :: Name -> [Var] -> Value
findVar a [] = error ("Can not find the name " ++ a ++ " in value list.")
findVar a ((d, e, f):xs) = if a == e then case f of Val x -> x else findVar a xs

do_operation :: Expr -> [Var] -> Value
do_operation (Get a)            s = findVar a s
do_operation (Val (TString _ )) s = error "do_operation function can not allow String Value."
do_operation (Val (TBool _ ))   s = error "do_operation function can not allow Bool Value."
do_operation (Val a)            s = a
do_operation (Add a b)          s = do_operation_IntandDouble ((do_operation a s), (do_operation b s)) Plus        
do_operation (Mul a b)          s = do_operation_IntandDouble ((do_operation a s), (do_operation b s)) Multiply       
do_operation (Mis a b)          s = do_operation_IntandDouble ((do_operation a s), (do_operation b s)) Minus       
do_operation (Div a b)          s = do_operation_IntandDouble ((do_operation a s), (do_operation b s)) Divide      
do_operation (Mod a b)          s = do_operation_IntandDouble ((do_operation a s), (do_operation b s)) Remainder

do_Bool :: Expb -> [Var] -> Bool
do_Bool (Blv_s a b)  s = case ((do_operation a s), (do_operation b s)) of ((TInt c), (TInt d)) -> c < d
                                                                          ((TDouble c), (TDouble d)) -> c < d
                                                                          _ -> error "Value not match. Only can compare two Int or two Double."
do_Bool (Blv_b a b)  s = case ((do_operation a s), (do_operation b s)) of ((TInt c), (TInt d)) -> c > d
                                                                          ((TDouble c), (TDouble d)) -> c > d
                                                                          _ -> error "Value not match. Only can compare two Int or two Double."
do_Bool (Blv_q a b)  s = case ((do_operation a s), (do_operation b s)) of ((TInt c), (TInt d)) -> c == d
                                                                          ((TDouble c), (TDouble d)) -> c == d
                                                                          ((TString c), (TString d)) -> c == d
                                                                          ((TBool c), (TBool d)) -> c == d
                                                                          _ -> error "Value not match. Only can compare two Int or two Double."
do_Bool (Blv_nq a b) s = case ((do_operation a s), (do_operation b s)) of ((TInt c), (TInt d)) -> c /= d
                                                                          ((TDouble c), (TDouble d)) -> c /= d
                                                                          ((TString c), (TString d)) -> c /= d
                                                                          ((TBool c), (TBool d)) -> c /= d
                                                                          _ -> error "Value not match. Only can compare two Int or two Double."
do_Bool (Blv_sq a b) s = case ((do_operation a s), (do_operation b s)) of ((TInt c), (TInt d)) -> c <= d
                                                                          ((TDouble c), (TDouble d)) -> c <= d
                                                                          _ -> error "Value not match. Only can compare two Int or two Double."
do_Bool (Blv_bq a b) s = case ((do_operation a s), (do_operation b s)) of ((TInt c), (TInt d)) -> c >= d
                                                                          ((TDouble c), (TDouble d)) -> c >= d
                                                                          _ -> error "Value not match. Only can compare two Int or two Double."
do_Bool (Bli a)      s = if a /= 0 then True else False
--do_Bool (Blb_q a b) s = (do_Bool a s) == (do_Bool b s)
--do_Bool (Blb_nq a b) s = (do_Bool a s) /= (do_Bool b s)

checkconstr :: Value -> Value -> Bool
checkconstr (TInt a) (TInt b) = True
checkconstr (TDouble a) (TDouble b) = True
checkconstr (TString a) (TString b) = True
checkconstr (TBool a) (TBool b) = True
checkconstr a b = False

updatelist :: Var -> [Var] -> Maybe [Var]
updatelist a         []           = Nothing
updatelist (a, b, c) ((d,e,f):xs) = if a == d && b == e then case (f, c) of (Val g, Val h) -> if (checkconstr g h) then Just ((d, e, c):xs) 
                                                                                              else error "Update result not match the Value."
                                                                            _ -> error ("Wrong value in Function Name : " ++ d ++ " Value Name : " ++ e)
                                    else case (updatelist (a, b, c) xs) of Just x -> Just ((d,e,f):(x))
                                                                           Nothing -> Nothing

checkset :: (Fname, Name) -> [Var] -> Bool
checkset a      []              = False
checkset (a, b) ((d, e, f):xs)  = if a == d && b == e then True else checkset (a, b) xs

-- Our print have not finish so we use error function to show you.
doCmd :: Cmd -> EnvironmentData -> Fname -> EnvironmentData
doCmd (Print a)       (v, s, f) n = case (do_operation a v) of (TInt ti) -> (v, (s ++ [show (ti)]), f)
                                                               (TDouble td) -> (v, (s ++ [show (td)]), f)
                                                               (TString ts) -> (v, (s ++ [ts]), f)
                                                               (TBool tb) -> case tb of True -> (v, (s ++ ["True"]), f)
                                                                                        False -> (v, (s ++ ["False"]), f)
doCmd (Set (a, b))    (v, s, f) n = if (checkset (n, a) v) then error ("Name : " ++ a ++ " in " ++ n ++ " value list. " ++ "Set command can not allow same name in sam function name.")
                                    else let 
                                             ans = do_operation b v 
                                         in 
                                             (((n, a, (Val (ans))):v), s, f)
                                                        
doCmd (Ifelse a b c)  (v, s, f) n = if (do_Bool a v) then doProg b (v, s, f) n
                                    else doProg c (v, s, f) n
doCmd (Update (a, b, c))    (v, s, f) n = case (updatelist (a, b, Val (do_operation c v)) v) of (Just x) -> (x, s, f)
                                                                                                (Nothing) -> error ("Value name " ++ b ++ " not in function " ++ n ++ " value list.")
--doCmd (Operation a)  s = do_operation a s
doCmd (For a b c d)   (v, s, f) n = 
    if (do_Bool b v) then 
       case b of 
           (Blv_s i j) ->  let 
                              (nv, ns, nf) = (doProg d (v, s, f) n)
                              add = do_operation_IntandDouble ((do_operation i nv), c) Plus
                              newresult = doCmd (Update (n, a, (Val add))) (nv, ns, nf) n
                           in 
                              doCmd (For a (Blv_s i j) c d) newresult n
           (Blv_q i j) ->  let 
                              (nv, ns, nf) = (doProg d (v, s, f) n)
                              add = do_operation_IntandDouble ((do_operation i nv), c) Plus
                              newresult = doCmd (Update (n, a, (Val add))) (nv, ns, nf) n
                           in 
                              doCmd (For a (Blv_q i j) c d) newresult n
           (Blv_nq i j) -> let 
                              (nv, ns, nf) = (doProg d (v, s, f) n)
                              add = do_operation_IntandDouble ((do_operation i nv), c) Plus
                              newresult = doCmd (Update (n, a, (Val add))) (nv, ns, nf) n
                           in 
                              doCmd (For a (Blv_nq i j) c d) newresult n
           (Blv_b i j) ->  let 
                              (nv, ns, nf) = (doProg d (v, s, f) n)
                              add = do_operation_IntandDouble ((do_operation i nv), c) Plus
                              newresult = doCmd (Update (n, a, (Val add))) (nv, ns, nf) n
                           in 
                              doCmd (For a (Blv_b i j) c d) newresult n
           (Blv_sq i j) -> let 
                              (nv, ns, nf) = (doProg d (v, s, f) n)
                              add = do_operation_IntandDouble ((do_operation i nv), c) Plus
                              newresult = doCmd (Update (n, a, (Val add))) (nv, ns, nf) n
                           in 
                              doCmd (For a (Blv_sq i j) c d) newresult n
           (Blv_bq i j) -> let 
                              (nv, ns, nf) = (doProg d (v, s, f) n)
                              add = do_operation_IntandDouble ((do_operation i nv), c) Plus
                              newresult = doCmd (Update (n, a, (Val add))) (nv, ns, nf) n
                           in 
                              doCmd (For a (Blv_bq i j) c d) newresult n
    else (v, s, f)

doCmd (While b d) (v, s, f) n =
    if (do_Bool b v) then 
       case b of 
           (Blv_s i j) ->  let 
                             result = (doProg d (v, s, f) n)
                           in 
                             doCmd (While (Blv_s i j) d) result n
           (Blv_q i j) ->  let 
                              result = (doProg d (v, s, f) n)
                           in 
                              doCmd (While (Blv_q i j) d) result n
           (Blv_nq i j) -> let 
                              result = (doProg d (v, s, f) n)
                           in 
                              doCmd (While (Blv_nq i j) d) result n
           (Blv_b i j) ->  let 
                              result = (doProg d (v, s, f) n)
                           in 
                              doCmd (While (Blv_b i j) d) result n
           (Blv_sq i j) -> let 
                              result = (doProg d (v, s, f) n)
                           in 
                              doCmd (While (Blv_sq i j) d) result n
           (Blv_bq i j) -> let 
                              result = (doProg d (v, s, f) n)
                           in 
                              doCmd (While (Blv_bq i j) d) result n
    else (v, s, f)

remove_function_val :: Fname -> [Var] -> (Fname, [Var])
remove_function_val a []             = (a, [])
remove_function_val a ((b, c, d):xs) = if a == b then 
                                          if c == "return" then let 
                                                                  (fn, s) = remove_function_val a xs
                                                                in
                                                                  (fn, ((fn, c, d):s))
                                          else remove_function_val a xs
                                       else (b, ((b, c, d):xs))

findfunction :: [(Fname, Prog)] -> Fname -> Prog
findfunction []          n = error "Function not set."
findfunction ((a, b):xs) n = if a == n then b
                             else findfunction xs n

callf :: [Var] -> EnvironmentData -> Fname -> EnvironmentData
callf []     s n = s
callf (x:xs) s n = callf xs (doCmd (Update x) s n) n

doProg :: Prog -> EnvironmentData -> Fname -> EnvironmentData
doProg []                        s         n = s
doProg ((SetFunction a b c):xs)  (v, s, f) n = doProg xs (v ++ b, s, ((a, c):f)) n
doProg ((CallFunction a b):xs)   (v, s, f) n = doProg xs (doProg (findfunction f a) (callf b (v, s, f) n) n) n
doProg ((Return a):xs)           (v, s, f) n = let ans = do_operation a v in doProg ((End n):[]) (((n, "return", (Val (ans))):v), s, f) n
doProg ((Begin a):xs)            s         n = doProg xs s a
doProg ((End a):xs)              (v, s, f) n = let (nn, ns) = (remove_function_val a v) in doProg xs (ns, s, f) nn
doProg (x:xs)                    s         n = doProg xs (doCmd x s n) n

start :: Prog -> IO ()
start [] = putStrLn "Nothing"
start a  = let (v, s, f) = (doProg a ([], [], []) "main") in putStrLn (intercalate "\n" s)