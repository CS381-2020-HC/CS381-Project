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
           | TError String    -- Error Info
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

-- Environment of data, a.k.a. all information
-- [Var]           -> All variable info
-- [String]        -> All info which we want to print
-- [(Fname, Prog)] -> All function info
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
          | Mul Expr Expr    -- (*)
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

-- To print the result
data OutPut = Env [Var]
            | Prt [String]

-------------------------------------- Test function start ---------------------------------------
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
-------------------------------------- Test function end ---------------------------------------


-------------------------------------- Example Start -------------------------------------------
-- This is for testing the 1071 and 462 euclidean algorithm
-- The inputs are 1071 and 462. The input can change in "Set ("euc_first" or "euc_second" function)"
euclidean_algorithm :: Prog
euclidean_algorithm = [
                        Set ("euc_first", (Val (VInt 1071))),      -- Change the input here
                        Set ("euc_second", (Val (VInt 462))),      -- Change the input here

                        SetFunction "gcd" 
                        [("gcd", "m", Val (VInt 0)),("gcd", "n", Val (VInt 0))]
                        [ Begin "gcd",
                          Set ("t", Val (VInt 1)),
                          While (Blv_nq (Get "t") (Val (VInt 0))) 
                                [
                                   Update ("gcd", "t", Mod (Get "m") (Get "n")),
                                   Update ("gcd", "m", Get "n"),
                                   Update ("gcd", "n", Get "t")
                                ],
                          Return (Get "m")
                        ],
                        CallFunction "gcd" [("gcd", "m", Get("euc_first")),("gcd", "n", Get ("euc_first"))],    
                        Set ("x", Get "return"),
                        Print (Get "x")
                      ]

-- This is for transfer the Celsius to Fahrenheit and Fahrenheit to Celsius
-- The inputs are both 50, and the input can change in "Set ("temp_f" or "temp_c" function)"
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
         CallFunction "Fib" [("Fib", "n", Get "fib_n")  ],
         Print (Get "return")
      ]

-- This is for testing the fibonacci twice, means input the answer of the fib to do the second times 
-- The input is 6 now, and the input can change in "Set ("fib_n" function)"
-- fib(6) = 8
-- fib(8) = 21
fib2 :: Prog
fib2     = [
         Set ("fib_n", (Val (VInt 6))),    -- Change the input here
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
         CallFunction "Fib" [("Fib", "n", Get "fib_n")  ],
         CallFunction "Fib" [("Fib", "n", Get "return")  ],
         Print (Get "return")
      ]

-------------------------------------- Example End -------------------------------------------

-- Do operation in Int, Double, and String
-- LeftRight -> Tuple of two argument
-- Oper      -> Which operation will they use
-- Value     -> Value after calculate
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
do_op_IDS (VDouble a, VDouble b) Remainder = TError "Mod only can input two Int."
do_op_IDS (VInt a, VDouble b)    Remainder = TError "Mod only can input two Int."
do_op_IDS (VDouble a, VInt b)    Remainder = TError "Mod only can input two Int."
do_op_IDS _                              a = case a of 
                                                Plus -> TError "Bool and TError can not do the Add operation."
                                                Multiply -> TError "String, Bool, List, and TError can not do the Mul operation."
                                                Minus -> TError "String, Bool, List, and TError can not do the Mis operation."
                                                Divide -> TError "String, Bool, List, and TError can not do the Div operation."
                                                Remainder -> TError "Mod only can input two Int."

-- Find Variable 
-- Name  -> Variable's name
-- [Var] -> Variable store list
-- Value -> Variable value
findVar :: Name -> [Var] -> Value
findVar a [] = TError ("Can not find the name " ++ a ++ " in value list.")
findVar a ((d, e, f):xs) = if a == e then case f of Val x -> x else findVar a xs

-- Do operation in all expression
-- Expr  -> All of expression
-- [Var] -> Keep variable list
-- Value -> Value after calculate
do_op :: Expr -> [Var] -> Value
do_op (Get a)            s = findVar a s
-- do_op (Val (VString _ )) s = error "do_op function can not allow String Value."
do_op (Val (VBool _ ))   s = TError "do_op function can not allow Bool Value."
do_op (Val a)            s = a
do_op (Add a b)          s = case ((do_op a s), (do_op b s))of 
                                 (VList a, VList b)     -> if checkconstr (VList a) (VList b) then VList (a ++ b) else TError "Two List cound not add together because the type not same."
                                 (VList (a:as), b)      -> if checkconstr a b then VList ((a:as) ++ [b]) else TError ((constostr b) ++ " could not match the list.")
                                 (a, VList (b:bs))      -> if checkconstr a b then VList (a:b:bs) else TError ((constostr a) ++ " could not match the list.")
                                 (a,b)                  -> do_op_IDS (a, b) Plus        
do_op (Mul a b)          s = do_op_IDS ((do_op a s), (do_op b s)) Multiply       
do_op (Mis a b)          s = do_op_IDS ((do_op a s), (do_op b s)) Minus       
do_op (Div a b)          s = do_op_IDS ((do_op a s), (do_op b s)) Divide      
do_op (Mod a b)          s = do_op_IDS ((do_op a s), (do_op b s)) Remainder

-- Do array operation
-- Expr  -> Find string and add
-- [Var] -> Keep variable list
-- Value -> Value after calculate
do_arrayoperation :: Expr -> [Var] -> Value
do_arrayoperation (Val (VString a))                         s = VString a
do_arrayoperation (Add (Val (VString a)) (Val (VString b))) s = VString (a ++ b)
do_arrayoperation _                                         s = TError "do_arrayoperation function type error"

-- Do boolean calculation
-- Expb       -> Find conditional expressions
-- [Var]      -> Keep variable list
-- Maybe Bool -> Maybe is for the type error, Bool is get the results of conditional expression
do_Bool :: Expb -> [Var] -> Maybe Bool
do_Bool (Blv_s a b)  s = case ((do_op a s), (do_op b s)) of 
                           ((VInt c), (VInt d))       -> Just (c < d)
                           ((VDouble c), (VDouble d)) -> Just (c < d)
                           _                          -> Nothing -- error "Value not match. Only can compare two Int or two Double."
do_Bool (Blv_b a b)  s = case ((do_op a s), (do_op b s)) of 
                           ((VInt c), (VInt d))       -> Just (c > d)
                           ((VDouble c), (VDouble d)) -> Just (c > d)
                           _                          -> Nothing -- error "Value not match. Only can compare two Int or two Double."
do_Bool (Blv_q a b)  s = case ((do_op a s), (do_op b s)) of 
                           ((VInt c), (VInt d))       -> Just (c == d)
                           ((VDouble c), (VDouble d)) -> Just (c == d)
                           ((VString c), (VString d)) -> Just (c == d)
                           ((VBool c), (VBool d))     -> Just (c == d)
                           _                          -> Nothing -- Nothing -- error "Value not match. Only can compare two Int or two Double."
do_Bool (Blv_nq a b) s = case ((do_op a s), (do_op b s)) of 
                           ((VInt c), (VInt d))       -> Just (c /= d)
                           ((VDouble c), (VDouble d)) -> Just (c /= d)
                           ((VString c), (VString d)) -> Just (c /= d)
                           ((VBool c), (VBool d))     -> Just (c /= d)
                           _                          -> Nothing -- error "Value not match. Only can compare two Int or two Double."
do_Bool (Blv_sq a b) s = case ((do_op a s), (do_op b s)) of 
                           ((VInt c), (VInt d))       -> Just (c <= d)
                           ((VDouble c), (VDouble d)) -> Just (c <= d)
                           _                          -> Nothing -- error "Value not match. Only can compare two Int or two Double."
do_Bool (Blv_bq a b) s = case ((do_op a s), (do_op b s)) of 
                           ((VInt c), (VInt d))       -> Just (c >= d)
                           ((VDouble c), (VDouble d)) -> Just (c >= d)
                           _                          -> Nothing -- error "Value not match. Only can compare two Int or two Double."
do_Bool (Bli a)      s = if a /= 0 then Just True else Just False
do_Bool (GetBool a)  s = Just a

-- Check the type of two value is same or not
checkconstr :: Value -> Value -> Bool
checkconstr (VInt a) (VInt b)             = True
checkconstr (VDouble a) (VDouble b)       = True
checkconstr (VString a) (VString b)       = True
checkconstr (VBool a) (VBool b)           = True
checkconstr (VList (a:as)) (VList (b:bs)) = checkconstr a b
checkconstr a b                           = False

-- Transfer the value to string, this function is for the error info
constostr :: Value -> String
constostr (VInt    _ ) = "VInt"
constostr (VDouble _ ) = "VDouble"
constostr (VString _ ) = "VString"
constostr (VBool   _ ) = "VBool"
constostr (VList   _ ) = "VList"
constostr (TError  _ ) = "TError"

-- Update list
-- Var                    -> The variable which we want to update
-- [Var]                  -> Variable list which in evironment data
-- [String]               -> The list which we want to print
-- Maybe ([Var],[String]) -> Return the updated info
updatelist :: Var -> [Var] -> [String] -> Maybe ([Var],[String])
updatelist a []                   s = Nothing
updatelist (a, b, c) ((d,e,f):xs) s = if a == d && b == e then 
                                       case (f, c) of 
                                          (Val g, Val h) -> if (checkconstr g h) then Just (((d, e, c):xs),s) 
                                                            else Just (((d, e, Val (TError "Update result not match the Value.")):xs),s++ ["TError : Update result not match the Value."])
                                          _              -> Nothing
                                      else 
                                       case (updatelist (a, b, c) xs s) of 
                                          Just (x,ns)  -> Just (((d,e,f):(x)),ns)
                                          Nothing      -> Nothing

-- Check the environment variable has same name or not
-- (Fname, Name) -> Function name and variable name
-- [Var]         -> Variable list
-- Bool          -> Exist same name or not
checkset :: (Fname, Name) -> [Var] -> Bool
checkset a []                  = False
checkset (a, b) ((d, e, f):xs) = if a == d && b == e then True else checkset (a, b) xs

-- Transfer list to string
-- [Value] -> List
-- String  -> String of the list
listtostring :: [Value] -> String
listtostring (a:[]) = case a of VInt    i -> (show (i))
                                VDouble d -> (show (d))
                                VString s -> s
                                VBool   b -> case b of 
                                                True -> "True"
                                                False -> "False"
                                VList   l -> (listtostring l)
                                TError  t -> "TError : " ++ t
listtostring (a:as) = case a of VInt i -> (show (i)) ++ ", " ++ (listtostring as)
                                VDouble d -> (show (d)) ++ ", " ++ (listtostring as)
                                VString s -> s ++ ", " ++ (listtostring as)
                                VBool b -> case b of True -> "True" ++ ", " ++ (listtostring as)
                                                     False -> "False" ++ ", " ++ (listtostring as)
                                VList l -> (listtostring l) ++ ", " ++ (listtostring as)
                                TError  t -> "TError : " ++ t ++ ", " ++ (listtostring as)

-- Do every Commend, such as "set, ifelse, update the variable, for ..."
-- Cmd     -> Commend
-- EnvData -> Original environment of data, a.k.a. all information
-- Fname   -> To know which cmd am I
-- EnvData -> Updated environment of data, a.k.a. all information
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
                                    (v, (s ++ [("Name : " ++ a ++ " in " ++ n ++ " value list. " ++ "Set command can not allow same name in sam function name.")]), f) 
                                 else 
                                    let 
                                       ans = do_op b v 
                                    in 
                                        case ans of 
                                            TError te -> (((n, a, (Val (ans))):v), (s ++ ["TError : " ++ te]), f)
                                            _         -> (((n, a, (Val (ans))):v), s, f)
                                                        
doCmd (Ifelse a b c) (v, s, f) n = case do_Bool a v of
                                    Just a -> if a then doProg b (v, s, f) n
                                              else doProg c (v, s, f) n
                                    _      -> (v, s ++ ["TError : do_Bool argument type error."], f)
doCmd (Update (a, b, c)) (v, s, f) n = case (updatelist (a, b, Val (do_op c v)) v s) of 
                                          Just (x,ns) -> (x, ns, f)
                                          Nothing -> (v, s ++ ["TError : " ++ "Value name " ++ b ++ " not in function " ++ n ++ " value list."], f)
doCmd (For a b c d) (v, s, f) n = case do_Bool b v of
                                    Just e -> if e then 
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
                                                   _            -> (v, (s ++ ["For only allow <,>,<=,>=,==,/= "]), f) 
                                              else (v, s, f)
                                    _      -> (v, s ++ ["TError : do_Bool argument type error."], f)

doCmd (While b d) (v, s, f) n = case do_Bool b v of
                                    Just a -> if a then 
                                                 let 
                                                   result = (doProg d (v, s, f) n)
                                                 in 
                                                   doCmd (While b d) result n
                                              else (v, s, f)
                                    _      -> (v, s ++ ["TError : do_Bool argument type error."], f)


--------------------------------- Syntex sugar start ------------------------------------------

-- for :: String -> Expb -> Value -> Prog -> Cmd
-- for s e v p = While e  

-- True 
true :: Expb
true = Blv_q (Val (VInt 0)) (Val (VInt 0))

-- False
false :: Expb
false = Blv_q (Val (VInt 0)) (Val (VInt 1))

-- Do negative in number
neg :: Value -> Value
neg (VInt    e) = VInt ((-1) * e)
neg (VDouble e) = VDouble ((-1) * e)
neg _           = TError "Neg function has wrong type"

-- Do "not" in two boolean
not :: Expb -> Expb
not e = case do_Bool e [] of 
          Just a -> if a then false else true
          _      -> false

-- Do "and" in two boolean
and :: Expb -> Expb -> Expb
and l r = case do_Bool l [] of 
            Just a -> if a then r else false
            _      -> false

-- Do "or" in two boolean
or :: Expb -> Expb -> Expb
or l r = case do_Bool l [] of 
            Just a -> if a then true else r
            _      -> false

--------------------------------- Syntex sugar end ------------------------------------------

-- Remove the variable in the function
-- Fname          -> Which function
-- [Var]          -> Variable Info
-- (Fname, [Var]) -> Output
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

-- Find function name is exist or not
-- [(Fname, Prog)] -> The list of funcion name and it's program
-- Fname           -> Function name which you want to find
-- Prog            -> If the function is exist, it will return the program, if it isn't exist, it will return Error
findfunc :: [(Fname, Prog)] -> Fname -> Prog
findfunc []          n = [Print (Val (TError "Function not set."))]
findfunc ((a, b):xs) n = if a == n then b
                         else findfunc xs n
-- Call function when you want to call function
callf :: [Var] -> EnvData -> Fname -> EnvData
callf []     s n = s
callf (x:xs) s n = callf xs (doCmd (Update x) s n) n

-- Do the program, included run the function. In other words, it uses in type of Begin.
-- Prog    -> The list of commend
-- EnvData -> Original environment of data, a.k.a. all information
-- Fname   -> Name of the function 
-- EnvData -> Updated environment of data, a.k.a. all information
doProg :: Prog -> EnvData -> Fname -> EnvData
doProg []                        s         n = s
doProg ((SetFunction a b c):xs)  (v, s, f) n = doProg xs (v ++ b, s, ((a, c):f)) n
doProg ((CallFunction a b):xs)   (v, s, f) n = doProg xs (doProg (findfunc f a) (callf b (v, s, f) n) n) n
doProg ((Return a):xs)           (v, s, f) n = let ans = do_op a v in doProg ((End n):[]) (((n, "return", (Val (ans))):v), s, f) n
doProg ((Begin a):xs)            s         n = doProg xs s a
doProg ((End a):xs)              (v, s, f) n = let (nn, ns) = (remove_func_val a v) in doProg xs (ns, s, f) nn
doProg (x:xs)                    s         n = doProg xs (doCmd x s n) n

-- Start the program
-- Prog  -> The program
-- IO () -> To print the result of the program
start :: Prog -> IO ()
start [] = putStrLn "Nothing"
start a  = let (v, s, f) = (doProg a ([], [], []) "main") in putStrLn (intercalate "\n" s)


