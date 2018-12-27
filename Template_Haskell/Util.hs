{-# LANGUAGE TemplateHaskell, DataKinds, KindSignatures #-} 

module Util where 

import Language.Haskell.TH
import Language.Haskell.TH.Syntax 
import Control.Monad

class TUnfold t where     
   tunfold :: t a -> [a]

data P1D a  = P1D  { c0 :: a } deriving (Show, Eq)
data P2D a  = P2D  { d0 :: a, d1 :: a } deriving (Show, Eq)
data Rec2 a = Rec2 { n0 :: a, n1 :: a}  deriving (Show, Eq)
data Rec3 a = Rec3 { m0 :: a, m1 :: a, m2 :: a } deriving (Show, Eq)

genInst_TUnfold :: Name -> DecsQ
genInst_TUnfold typename = do
   TyConI (DataD _ _ _ _ [RecC conName fields] _) <- reify typename
   vars <-  replicateM (length fields) (newName "x") 
   {- return $ [InstanceD Nothing [] (AppT (ConT ''TUnfold) (ConT name))
                       [FunD ('tunfold) $ [Clause [ConP conName [VarP x,VarP y]] 
                                                  (NormalB $ ListE [VarE x,VarE y]) [] ]]]  -} 
   [d| instance TUnfold $(conT typename)  where 
          tunfold $(conP conName (map varP vars)) = $(listE $ map varE vars) |]
         -- we need to use DataKinds to be able to use ConT 

