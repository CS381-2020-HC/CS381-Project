module Main where

import Data.List

data Expi = GetInt
          | Lit Int
          | Add Expi Expi
          | Mul Expi Expi
          | Mis Expi Expi
          | Div Expi 

data Expb = GetBool
          | Bl Bool
          | Bli Expi Expi
          | Blb Expb Expb

addInt :: Expi -> Int
addInt = undefined