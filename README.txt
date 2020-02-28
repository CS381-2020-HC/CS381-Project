Name                       ONID
Sheng-Po Huang             huangsh
Yue Chen                   chenyu6
Shih-Chao Hung             hungshi
Tianhong Huang             huantian

Our language’s name is Modified-C. The paradigm of our language is Imperative languages. We decide to include the scope and lifetime of the variable.

Our language implementation should be intended to be run from GHCi. main.hs should be loaded.

These are two kinds test example:
First kind is used to test all:

testall :: Prog
testall = [
         
         Set ("i", Val (TInt 0)),
         Ifelse (Bli_s (Get "i")
                (Mis (Val (TInt 200)) (Val (TInt 100))))
                [
                    Set ("j", Val (TInt 0)),
                    For ("i")
                        (Bli_s (Get "i") (Val (TInt 10)))
                        (TInt 2)
                        [
                          (Update ("main", "j", (Add (Get "i") (Val (TInt 1)))))
                        ]
                ]
                [
                    While (Bli_s (Get "i") (Val (TInt 10)))
                          [(Update ("main", "i", (Add (Get "i") (Val (TInt 1)))))],
                    Set ("j", Get "i")
                ]
         ]

The way to use this test is: doProg testall [] "main"
This is a good test for all functions. This example includes all functions. This ideal output of this test.
The ideal output:
[("main","j",Val (TInt 9)),("main","i",Val (TInt 10))]

These are tests for special funtions:

For-loop:

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

The way to use it is: doProg testFor [] "main"
Ideal output:
[("main","j",Val (TInt 10)),("main","i",Val (TInt 10))]


While loop:

testWhile :: Prog
testWhile = [
           (Set ("i", Val (TInt 0)))
           ,
           (While (Bli_s (Get "i") (Val (TInt 10)))
                  [(Update ("main", "i", (Add (Get "i") (Val (TInt 1)))))]
           )]

The way to use it is: doProg testFor [] "main"
Ideal result:
[("main","i",Val (TInt 10))]

Conditional :

testIfElse = [
		(Set ("i", Val (TInt 0))
		,
		Ifelse (Bli_s (Get "i") [
		(Update ("main", "I", (Add (Get "i") (Val (TInt 1)))))
		]
The way to use it is: doProg testIfElse [] "main"
Ideal result:
[("main","i",Val (TInt 1))]
