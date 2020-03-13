Name                       ONID
Sheng-Po Huang             huangsh
Yue Chen                   chenyu6
Shih-Chao Hung             hungshi
Tianhong Huang             huantian

Our language’s name is Modified-C. The paradigm of our language is Imperative languages. The interesting thing is we put variable and function together . We do both for-loop and while-loop. We use end to realize scope and lifecycle.


Our language implementation should be intended to be run from GHCi. main.hs should be loaded.
*All examples need to use "$start [example name]" to run.
*For example: $ start euclidean_algorithm


All examples has been saved in main.hs
These are four good test examples which are small project:

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
example result:1071

-- This is for testing the fibonacci twice, means input the answer of the fib to do the second times 
-- The input is 6 now, and the input can change in "Set ("fib_n" function)"
-- fib(6) = 8
-- fib(8) = 21
fib2 :: Prog
fib2 = [
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
example result:21

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
example result:55

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
example result:
C:10.0
F:122.0

There are three bad examples:

-- This example shows error message 
-- because Mod (a.k.a Remainder) can't accept two different type argument
bad_1 :: Prog
bad_1 = [
          Print (Mod (Val (VDouble 3.6)) (Val (VInt 2))) -- Can not accept type double to do Mod function
        ]
example result:
TError:Mod only can input two Int

-- This example shows error message 
-- because this language can not accept set variable twice in a same name.
-- Instead, you should use Update function.
bad_2 :: Prog
bad_2 = [
            Set ("i", Val(VInt 2)),
            Set ("i", Val(VString "Hello")),  -- Can not accept set the existed variable. 
            Print (Get "i")
        ]
example result:
TError : While : do_Bool argument type error.TError : Can not find the name return in value list.
2

-- This example shows error message 
-- because this language can not accept String type in Boolean conditional expressions.
-- Instead, it only accepts Int and Double, and also some of Bool and String .
bad_3 :: Prog
bad_3 = [
            While (Blv_q (Val (VString "a")) (Val (VInt 0))) -- Can not compare String and Int is equal or not
            [
               Set ("i" , Val (VString "Hello"))
            ],
            Print (Get "return")
        ]
example result:
TError:Mod only can input two Int
