{-# LANGUAGE DerivingVia #-} 

newtype Wrap t = Wrap {getVal :: t} 
instance Functor Wrap where 
   fmap f (Wrap x) = Wrap $ f x 
instance Applicative Wrap where 
   pure = Wrap 
   (Wrap f) <*> (Wrap x) = pure $ f x 
instance Monad Wrap where 
   (Wrap x) >>= f = f x
   return = Wrap 

newtype BaseImm a = BaseImm {unBaseImm :: a} 
   deriving (Show,Eq,Ord) 
   deriving (Functor, Applicative,Monad) via Wrap
