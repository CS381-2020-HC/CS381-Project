module Man where
-- At First we use Main for module name, but it will get wrong so we change to this name.

import Data.List
import Data.Data
import Data.Function

-- Prog is the Value of list Cmd.
type Prog = [Cmd]

-- Define a Value data for the Int, Double, String, Bool.
-- We put the Float Value and Double Value together.
data Value = VInt Int         -- Int
           | VDouble Double   -- Float and Double
           | VString String   -- String
           | VBool Bool       -- Bool
           | VList [Value]    -- List
           | TError           -- not designe yet.
           deriving (Eq, Show)

-- Fname is the Function Name.
type Fname = String

-- Name is the variable name.
type Name = String

-- Define the value list base Value, and it include Function name, value name, and the Expr Value to do the operation.
type Var = (Fname, Name, Expr)

-- This Value will change the name in the fucture.
-- This Value just for know the two value Value in do_op_IDS.
type LeftRight = (Value, Value)  -- will rewrite the Value name

type EnvData = ([Var], [String], [(Fname, Prog)])

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
data Expb = GetBool Bool
          | Bli Int           -- Will rewrite
          | Blv_s Expr Expr   -- Left Smaller then Right.
          | Blv_q Expr Expr   -- Left Equal with Right.
          | Blv_nq Expr Expr  -- Left not Equal with Right.
          | Blv_b Expr Expr   -- Left Bigger then Right.
          | Blv_sq Expr Expr  -- Left Smaller and Equal then Right.
          | Blv_bq Expr Expr  -- Left Bigger and Equal then Right.
          deriving (Eq, Show)

data OutPut = Env [Var]
            | Prt [String]

testoperation :: Expr
testoperation = Add (Val (VInt 2)) (Mul (Val (VInt 6))(Val (VInt 3)))

testcondiction :: Expb
testcondiction = Blv_s (Add (Val (VInt 2)) (Mul (Val (VInt 6))(Val (VInt 3)))) (Val (VInt 21))

-- test2 :: Prog
-- test2 = [For (VInt 0) (Blv_s (Val (VInt 0)) (Val (VInt 10))) (VInt 1) [ Operation (Add Get (Val (VInt (-1)))) ] ]

testval :: EnvData
testval = ([("main","t1",Val (VInt 10)),("main","t2",Val (VDouble 5.8)),("main","t3",Val (VString "123")),("main","t4",Val (VBool True))], [], [])

testFor :: Prog
testFor = [
          (Set ("i", Val (VInt 0)))
          ,
          (Set ("j", Val (VInt 0)))
          ,
          (For ("i")
               (Blv_s (Get "i") (Val (VInt 10))) 
               (VInt 1)
               [(Update ("main", "j", (Add (Get "i") (Val (VInt 1)))))] 
          )]

testWhile :: Prog
testWhile = [
            (Set ("i", Val (VInt 0)))
            ,
            (While (Blv_s (Get "i") (Val (VInt 10))) 
                   [(Update ("main", "i", (Add (Get "i") (Val (VInt 1)))))] 
            ),
            Print (Get "i")]

testset :: Prog
testset = [(Set ("j", (Add (Get "i") (Val (VInt 1))))),(Set ("j", (Add (Get "i") (Val (VInt 1))))),(Set ("j", (Add (Get "i") (Val (VInt 1)))))]

testadd :: Value
testadd = let (v, s, f) = (doProg testset testval "For") in do_op_IDS ((do_op (Get "i") v), (VInt 1)) Plus

testIfElse :: Prog
testIfElse = [
             Set ("i", Val (VInt 0)),
             Ifelse (Blv_s (Get "i")
                    (Mis (Val (VInt 200)) (Val (VInt 100))))
                    [
                        (Update ("main", "i", (Add (Get "i") (Val (VInt 1)))))
                    ]
                    [
                        (Update ("main", "i", (Add (Get "i") (Val (VInt 2)))))
                    ]
             ]

testStringAdd :: Prog
testStringAdd = [Print (Add (Val (VString "Hello ")) (Add (Val (VString "World!")) (Val (VString " :)"))))]

testListadd :: Prog
testListadd = [
              Set ("i", (Add (Val (VList [VInt 0,VInt 1])) (Val (VInt 2)))),
              Set ("j", (Add (Val (VInt 5)) (Get "i"))),
              Set ("k", (Add (Get "i") (Get "j"))),
              Print (Get "i"),
              Print (Get "j"),
              Print (Get "k")
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
          Set ("i", Val (VInt 0)),
          Begin "ifelse",
          Ifelse (Blv_s (Get "i") 
                 (Mis (Val (VInt 200)) (Val (VInt 100)))) 
                 [
                     Set ("j", Val (VInt 0)),
                     Begin "for",
                     For ("i") 
                         (Blv_s (Get "i") (Val (VInt 10))) 
                         (VInt 2) 
                         [
                           (Update ("ifelse", "j", (Add (Get "i") (Val (VInt 1)))))
                         ],
                     End "for"
                 ] 
                 [
                     Begin "while",
                     While (Blv_s (Get "i") (Val (VInt 10))) 
                           [(Update ("ifelse", "i", (Add (Get "i") (Val (VInt 1)))))],
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
                        Set ("euc_first", (Val (VInt 1071))),      -- Change the input here
                        Set ("euc_second", (Val (VInt 462))),      -- Change the input here

                        SetFunction "gcd" 
                        [("gcd", "m", Val (VInt 0)),("gcd", "n", Val (VInt 0))]
                        [Begin "gcd",
                        Set ("t", Val (VInt 1)),
                        While (Blv_nq (Get "t") (Val (VInt 0))) 
                              [
                                 Update ("gcd", "t", Mod (Get "m") (Get "n")),
                                 Update ("gcd", "m", Get "n"),
                                 Update ("gcd", "n", Get "t")
                              ],
                        Return (Get "m")],
                        CallFunction "gcd" [("gcd", "m", Get("euc_first")),("gcd", "n", Get ("euc_first"))],    
                        Set ("x", Get "return"),
                        Print (Get "x")
                      ]

-- This is for transfer the Celsius to Fahrenheit and Fahrenheit to Celsius
-- the inputs are both 50, and the input can change in "Set ("temp_f" or "temp_c" function)"
temp_C_F_transfer :: Prog         
temp_C_F_transfer =  [
                        Set ("temp_f", (Val (VDouble 50))),      -- Change the input here
                        Set ("temp_c", (Val (VDouble 50))),      -- Change the input here
                        SetFunction "FtoC"
                        [("FtoC", "f", Val (VDouble 0))]
                        [
                           Begin "FtoC",
                           Return (Div (Mul (Mis (Get "f") (Val (VInt 32))) (Val (VInt 5))) (Val (VInt 9)))
                        ],
                        CallFunction "FtoC" [("FtoC", "f", Get "temp_f")],
                        Print (Add (Val(VString "C:")) (Get "return")),
                        SetFunction "CtoF"
                        [("CtoF", "c", Val (VDouble 0))]
                        [
                           Begin "CtoF",
                           Return (Add (Mul (Val (VDouble 1.8)) (Get "c")) (Val (VInt 32)))
                        ],
                        CallFunction "CtoF" [("CtoF", "c", Get "temp_c")],
                        Print (Add (Val(VString "F:")) (Get "return"))
                     ]

-- This is for testing the fibonacci 
-- The input is 10 now, and the input can change in "Set ("fib_n" function)"
fib :: Prog
fib = [
         Set ("fib_n", (Val (VInt 10))),    -- Change the input here
         SetFunction "Fib"
         [ 
            ("Fib", "n", Val (VInt 0)) 
         ]
         [
            Begin "Fib", 
            Set ("i", Val (VInt 1)),
            Set ("first", Val (VInt 0)),
            Set ("second", Val (VInt 0)),
            Set ("temp", Val (VInt 0)  ),
            For ("i") (Blv_sq (Get "i") (Get "n")) (VInt 1) 
            [
               Ifelse (Blv_q (Get "i") (Val (VInt 1))) 
               [
                  Update ("Fib", "second", Val(VInt 1))
               ] [
                  Update ("Fib", "temp", Get "second"),
                  Update ("Fib", "second", Add (Get "first") (Get "second")),
                  Update ("Fib", "first", Get "temp")
               ]
            ],
            Return (Get "second")
         ],
         CallFunction "Fib" [("Fib", "n", Get "fib_n")  ],                    -- set the input x in (VInt x)
         Print (Get "return")
      ]

--test :: Expr
--test = Add (Val (VInt 2)) (Mul (Val (VInt 6))(Val (VInt 3)))

do_op_IDS :: LeftRight -> Oper -> Value
do_op_IDS (VInt a, VInt b)            Plus = VInt (a + b) 
do_op_IDS (VDouble a, VDouble b)      Plus = VDouble (a + b)
do_op_IDS (VInt a, VDouble b)         Plus = VDouble ((fromIntegral a) + b)
do_op_IDS (VDouble a, VInt b)         Plus = VDouble (a + (fromIntegral b))
do_op_IDS (VString a, VString b)      Plus = VString (a ++ b)
do_op_IDS (VString a, VInt b)         Plus = VString (a ++ show(b))
do_op_IDS (VString a, VDouble b)      Plus = VString (a ++ show(b))
do_op_IDS (VInt a, VString b)         Plus = VString (show(a) ++ b)
do_op_IDS (VDouble a, VString b)      Plus = VString (show(a) ++ b)
do_op_IDS (VInt a, VInt b)           Minus = VInt (a - b) 
do_op_IDS (VDouble a, VDouble b)     Minus = VDouble (a - b)
do_op_IDS (VInt a, VDouble b)        Minus = VDouble ((fromIntegral a) - b)
do_op_IDS (VDouble a, VInt b)        Minus = VDouble (a - (fromIntegral b))
do_op_IDS (VInt a, VInt b)        Multiply = VInt (a * b) 
do_op_IDS (VDouble a, VDouble b)  Multiply = VDouble (a * b)
do_op_IDS (VInt a, VDouble b)     Multiply = VDouble ((fromIntegral a) * b)
do_op_IDS (VDouble a, VInt b)     Multiply = VDouble (a * (fromIntegral b))
do_op_IDS (VInt a, VInt b)          Divide = VInt (a `div` b) 
do_op_IDS (VDouble a, VDouble b)    Divide = VDouble (a / b)
do_op_IDS (VInt a, VDouble b)       Divide = VDouble ((fromIntegral a) / b)
do_op_IDS (VDouble a, VInt b)       Divide = VDouble (a / (fromIntegral b))
do_op_IDS (VInt a, VInt b)       Remainder = VInt (a `mod` b) 
do_op_IDS (VDouble a, VDouble b) Remainder = error "Mod only can input two Int."
do_op_IDS (VInt a, VDouble b)    Remainder = error "Mod only can input two Int."
do_op_IDS (VDouble a, VInt b)    Remainder = error "Mod only can input two Int."
do_op_IDS _                              a = case a of 
                                                Plus -> error "Bool and TError can not do the Add operation."
                                                Multiply -> error "String, Bool, List, and TError can not do the Mul operation."
                                                Minus -> error "String, Bool, List, and TError can not do the Mis operation."
                                                Divide -> error "String, Bool, List, and TError can not do the Div operation."
                                                Remainder -> error "Mod only can input two Int."

findVar :: Name -> [Var] -> Value
findVar a [] = error ("Can not find the name " ++ a ++ " in value list.")
findVar a ((d, e, f):xs) = if a == e then case f of Val x -> x else findVar a xs

do_op :: Expr -> [Var] -> Value
do_op (Get a)            s = findVar a s
-- do_op (Val (VString _ )) s = error "do_op function can not allow String Value."
do_op (Val (VBool _ ))   s = error "do_op function can not allow Bool Value."
do_op (Val a)            s = a
do_op (Add a b)          s = case ((do_op a s), (do_op b s))of 
                                 (VList a, VList b)     -> if checkconstr (VList a) (VList b) then VList (a ++ b) else error "Two List cound not add together because the type not same."
                                 (VList (a:as), b)      -> if checkconstr a b then VList ((a:as) ++ [b]) else error ((constostr b) ++ " could not match the list.")
                                 (a, VList (b:bs))      -> if checkconstr a b then VList (a:b:bs) else error ((constostr a) ++ " could not match the list.")
                                 (a,b)                  -> do_op_IDS (a, b) Plus        
do_op (Mul a b)          s = do_op_IDS ((do_op a s), (do_op b s)) Multiply       
do_op (Mis a b)          s = do_op_IDS ((do_op a s), (do_op b s)) Minus       
do_op (Div a b)          s = do_op_IDS ((do_op a s), (do_op b s)) Divide      
do_op (Mod a b)          s = do_op_IDS ((do_op a s), (do_op b s)) Remainder

do_arrayoperation :: Expr -> [Var] -> Value
do_arrayoperation (Val (VString a))                         s = VString a
do_arrayoperation (Add (Val (VString a)) (Val (VString b))) s = VString (a ++ b)
do_arrayoperation _                                         s = error "do_arrayoperation function type error"

do_Bool :: Expb -> [Var] -> Bool
do_Bool (Blv_s a b)  s = case ((do_op a s), (do_op b s)) of 
                           ((VInt c), (VInt d))       -> c < d
                           ((VDouble c), (VDouble d)) -> c < d
                           _                          -> error "Value not match. Only can compare two Int or two Double."
do_Bool (Blv_b a b)  s = case ((do_op a s), (do_op b s)) of 
                           ((VInt c), (VInt d))       -> c > d
                           ((VDouble c), (VDouble d)) -> c > d
                           _                          -> error "Value not match. Only can compare two Int or two Double."
do_Bool (Blv_q a b)  s = case ((do_op a s), (do_op b s)) of 
                           ((VInt c), (VInt d))       -> c == d
                           ((VDouble c), (VDouble d)) -> c == d
                           ((VString c), (VString d)) -> c == d
                           ((VBool c), (VBool d))     -> c == d
                           _                          -> error "Value not match. Only can compare two Int or two Double."
do_Bool (Blv_nq a b) s = case ((do_op a s), (do_op b s)) of 
                           ((VInt c), (VInt d))       -> c /= d
                           ((VDouble c), (VDouble d)) -> c /= d
                           ((VString c), (VString d)) -> c /= d
                           ((VBool c), (VBool d))     -> c /= d
                           _                          -> error "Value not match. Only can compare two Int or two Double."
do_Bool (Blv_sq a b) s = case ((do_op a s), (do_op b s)) of 
                           ((VInt c), (VInt d))       -> c <= d
                           ((VDouble c), (VDouble d)) -> c <= d
                           _                          -> error "Value not match. Only can compare two Int or two Double."
do_Bool (Blv_bq a b) s = case ((do_op a s), (do_op b s)) of 
                           ((VInt c), (VInt d))       -> c >= d
                           ((VDouble c), (VDouble d)) -> c >= d
                           _                          -> error "Value not match. Only can compare two Int or two Double."
do_Bool (Bli a)      s = if a /= 0 then True else False
do_Bool (GetBool a)  s = a

checkconstr :: Value -> Value -> Bool
checkconstr (VInt a) (VInt b)             = True
checkconstr (VDouble a) (VDouble b)       = True
checkconstr (VString a) (VString b)       = True
checkconstr (VBool a) (VBool b)           = True
checkconstr (VList (a:as)) (VList (b:bs)) = checkconstr a b
checkconstr a b                           = False

constostr :: Value -> String
constostr (VInt    _ ) = "VInt"
constostr (VDouble _ ) = "VDouble"
constostr (VString _ ) = "VString"
constostr (VBool   _ ) = "VBool"
constostr (VList   _ ) = "VList"
constostr (TError    ) = "TError"

updatelist :: Var -> [Var] -> Maybe [Var]
updatelist a []                   = Nothing
updatelist (a, b, c) ((d,e,f):xs) = if a == d && b == e then 
                                       case (f, c) of 
                                          (Val g, Val h) -> if (checkconstr g h) then Just ((d, e, c):xs) 
                                                            else error "Update result not match the Value."
                                          _              -> error ("Wrong value in Function Name : " ++ d ++ " Value Name : " ++ e)
                                    else 
                                       case (updatelist (a, b, c) xs) of 
                                          Just x  -> Just ((d,e,f):(x))
                                          Nothing -> Nothing

checkset :: (Fname, Name) -> [Var] -> Bool
checkset a []                  = False
checkset (a, b) ((d, e, f):xs) = if a == d && b == e then True else checkset (a, b) xs

listtostring :: [Value] -> String
listtostring (a:[]) = case a of VInt    i -> (show (i))
                                VDouble d -> (show (d))
                                VString s -> s
                                VBool   b -> case b of 
                                                True -> "True"
                                                False -> "False"
                                VList   l -> (listtostring l)
listtostring (a:as) = case a of VInt i -> (show (i)) ++ ", " ++ (listtostring as)
                                VDouble d -> (show (d)) ++ ", " ++ (listtostring as)
                                VString s -> s ++ ", " ++ (listtostring as)
                                VBool b -> case b of True -> "True" ++ ", " ++ (listtostring as)
                                                     False -> "False" ++ ", " ++ (listtostring as)
                                VList l -> (listtostring l) ++ ", " ++ (listtostring as)

-- Our print have not finish so we use error function to show you.
doCmd :: Cmd -> EnvData -> Fname -> EnvData
doCmd (Print a) (v, s, f) n = case (do_op a v) of 
                                 (VInt ti)    -> (v, (s ++ [show (ti)]), f)
                                 (VDouble td) -> (v, (s ++ [show (td)]), f)
                                 (VString ts) -> (v, (s ++ [ts]), f)
                                 (VList tl)   -> (v, (s ++ ["[" ++ (listtostring tl) ++ "]"]), f)
                                 (VBool tb)   -> case tb of 
                                                   True  -> (v, (s ++ ["True"]), f)
                                                   False -> (v, (s ++ ["False"]), f)
doCmd (Set (a, b)) (v, s, f) n = if (checkset (n, a) v) then 
                                    error ("Name : " ++ a ++ " in " ++ n ++ " value list. " ++ "Set command can not allow same name in sam function name.")
                                 else 
                                    let 
                                       ans = do_op b v 
                                    in 
                                       (((n, a, (Val (ans))):v), s, f)
                                                        
doCmd (Ifelse a b c) (v, s, f) n = if (do_Bool a v) then doProg b (v, s, f) n
                                   else doProg c (v, s, f) n
doCmd (Update (a, b, c)) (v, s, f) n = case (updatelist (a, b, Val (do_op c v)) v) of 
                                          (Just x) -> (x, s, f)
                                          (Nothing) -> error ("Value name " ++ b ++ " not in function " ++ n ++ " value list.")
--doCmd (Operation a)  s = do_op a s
doCmd (For a b c d) (v, s, f) n = if (do_Bool b v) then 
                                    case b of 
                                       (Blv_s i j) ->  let 
                                                            (nv, ns, nf) = (doProg d (v, s, f) n)
                                                            add = do_op_IDS ((do_op i nv), c) Plus
                                                            newresult = doCmd (Update (n, a, (Val add))) (nv, ns, nf) n
                                                       in 
                                                            doCmd (For a (Blv_s i j) c d) newresult n
                                       (Blv_q i j) ->  let 
                                                            (nv, ns, nf) = (doProg d (v, s, f) n)
                                                            add = do_op_IDS ((do_op i nv), c) Plus
                                                            newresult = doCmd (Update (n, a, (Val add))) (nv, ns, nf) n
                                                       in 
                                                            doCmd (For a (Blv_q i j) c d) newresult n
                                       (Blv_nq i j) -> let 
                                                            (nv, ns, nf) = (doProg d (v, s, f) n)
                                                            add = do_op_IDS ((do_op i nv), c) Plus
                                                            newresult = doCmd (Update (n, a, (Val add))) (nv, ns, nf) n
                                                       in 
                                                            doCmd (For a (Blv_nq i j) c d) newresult n
                                       (Blv_b i j) ->  let 
                                                            (nv, ns, nf) = (doProg d (v, s, f) n)
                                                            add = do_op_IDS ((do_op i nv), c) Plus
                                                            newresult = doCmd (Update (n, a, (Val add))) (nv, ns, nf) n
                                                       in 
                                                            doCmd (For a (Blv_b i j) c d) newresult n
                                       (Blv_sq i j) -> let 
                                                            (nv, ns, nf) = (doProg d (v, s, f) n)
                                                            add = do_op_IDS ((do_op i nv), c) Plus
                                                            newresult = doCmd (Update (n, a, (Val add))) (nv, ns, nf) n
                                                       in 
                                                            doCmd (For a (Blv_sq i j) c d) newresult n
                                       (Blv_bq i j) -> let 
                                                            (nv, ns, nf) = (doProg d (v, s, f) n)
                                                            add = do_op_IDS ((do_op i nv), c) Plus
                                                            newresult = doCmd (Update (n, a, (Val add))) (nv, ns, nf) n
                                                       in 
                                                            doCmd (For a (Blv_bq i j) c d) newresult n
                                       _            -> error "For only allow <,>,<=,>=,==,/= "
                                  else (v, s, f)

doCmd (While b d) (v, s, f) n =
    if (do_Bool b v) then 
       let 
         result = (doProg d (v, s, f) n)
       in 
         doCmd (While b d) result n
    else (v, s, f)

--syntex sugar
true :: Expb
true = Blv_q (Val (VInt 0)) (Val (VInt 0))

false :: Expb
false = Blv_q (Val (VInt 0)) (Val (VInt 1))

neg :: Value -> Value
neg (VInt    e) = VInt ((-1) * e)
neg (VDouble e) = VDouble ((-1) * e)
neg _ = TError

not :: Expb -> Expb
not e = if (do_Bool e []) then false else true

and :: Expb -> Expb -> Expb
and l r = if (do_Bool l []) then r else false

or :: Expb -> Expb -> Expb
or l r = if (do_Bool l []) then true else r

remove_func_val :: Fname -> [Var] -> (Fname, [Var])
remove_func_val a []             = (a, [])
remove_func_val a ((b, c, d):xs) = if a == b then 
                                       if c == "return" then 
                                          let 
                                             (fn, s) = remove_func_val a xs
                                          in
                                             (fn, ((fn, c, d):s))
                                       else remove_func_val a xs
                                    else (b, ((b, c, d):xs))

findfunc :: [(Fname, Prog)] -> Fname -> Prog
findfunc []          n = error "Function not set."
findfunc ((a, b):xs) n = if a == n then b
                         else findfunc xs n

callf :: [Var] -> EnvData -> Fname -> EnvData
callf []     s n = s
callf (x:xs) s n = callf xs (doCmd (Update x) s n) n

doProg :: Prog -> EnvData -> Fname -> EnvData
doProg []                        s         n = s
doProg ((SetFunction a b c):xs)  (v, s, f) n = doProg xs (v ++ b, s, ((a, c):f)) n
doProg ((CallFunction a b):xs)   (v, s, f) n = doProg xs (doProg (findfunc f a) (callf b (v, s, f) n) n) n
doProg ((Return a):xs)           (v, s, f) n = let ans = do_op a v in doProg ((End n):[]) (((n, "return", (Val (ans))):v), s, f) n
doProg ((Begin a):xs)            s         n = doProg xs s a
doProg ((End a):xs)              (v, s, f) n = let (nn, ns) = (remove_func_val a v) in doProg xs (ns, s, f) nn
doProg (x:xs)                    s         n = doProg xs (doCmd x s n) n

start :: Prog -> IO ()
start [] = putStrLn "Nothing"
start a  = let (v, s, f) = (doProg a ([], [], []) "main") in putStrLn (intercalate "\n" s)


