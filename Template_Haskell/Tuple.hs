{-# LANGUAGE TemplateHaskell, FlexibleInstances, DataKinds, KindSignatures #-} 


module Tuple where 

import Util 
import Language.Haskell.TH.Syntax 

$(genInst_TUnfold ''P1D)
$(genInst_TUnfold ''P2D)
$(genInst_TUnfold ''Rec2)
$(genInst_TUnfold ''Rec3)
