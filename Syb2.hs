{-# LANGUAGE DeriveDataTypeable #-}

module Syb2 where 

import Data.Generics
import Data.Data

data Company = C [Dept] deriving (Typeable,Data, Show)
data Dept = D Name Manager [SubUnit] deriving (Typeable, Data,Show)
data SubUnit = PU Employee | DU Dept deriving (Typeable, Data,Show)
data Employee = E Person Salary deriving (Typeable, Data,Show)
data Person = P Name Address deriving (Typeable, Data,Show)
data Salary = S Float deriving (Typeable, Data,Show)
type Manager = Employee 
type Name = String
type Address = String

comp = C [D "Sales" (E (P "Mohsen" "homewood") (S 100))
            [PU (E (P "Salwa" "farimount") (S 50)) , PU (E (P "Nadia" "Amelia") (S 50))],
          D "Design" (E (P "Monna" "BlinkBonnie") (S 1000))
            [PU (E (P "Omar" "BB") (S 500)), PU (E (P "Lina" "BB") (S 500))]]

incSal :: Salary -> Salary
incSal (S s) = S (s*2.0) 

incCompSals :: Typeable a => (a -> a) -> Company -> Company
incCompSals f company = everywhere (mkT incSal) company 

