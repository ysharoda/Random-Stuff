{-# LANGUAGE GADTs, StandaloneDeriving, 
    ScopedTypeVariables, 
    TypeOperators, DataKinds, PolyKinds,
    PartialTypeSignatures #-}

module Gadt where

{- 
GADTs allow for pattern matching over the parameteried types 
For example:
the type 

Either a b = Left a | Right b 
```
The user sends a specific a and b, they get the same types. There is no way to give more details about the types ot to take decisions based on them. 
When employing ADTs, we can write something like (source: GADTs for dummies: https://wiki.haskell.org/GADTs_for_dummies) 
-} 

data T a where
  D1 :: Int -> T String
  D2 :: T Bool
  D3 :: (a,a) -> T [a]

{- 
Notice how the different constructors can take different arguments, and the type of the output is based on what constructor is used and the types passed to it. 
-}

{- 
Another good example, from: Fun with Phantom Types (http://www.cs.ox.ac.uk/ralf.hinze/publications/With.pdf) 
Defining a language for terms
-}
data Term' t =
    Zero'
  | Succ' (Term' Int) -- this should return Term Int 
  | Pred' (Term' Int) -- this should return Term Int 
  | IsZero' (Term' Int) -- this should return Term Bool
-- there is no way of specifying the return types being different (Term Int or Term Bool), without using GADTS. Therefore, given this type an interpreter/evaluator can go wrong. The only case when we have the write answer is if statments
   | If' (Term' Bool) (Term' t) (Term' t)  

{- This definition allow us to write something like: 
"IsZero $ IsZero $ Succ Zero"
which produces runtime error, but not a type error. 

Also, defining the following eval function won't work. Because `eval (Succ x)` returns a value of type t, not Int, we cannot perform +1 on it. 

eval :: Term t -> t
eval (Zero) = 0
eval (Succ x) = (eval x) + 1 -- this given an error. Who knows (eval x) has the type Int, when the type signature only says it has the type t 
eval (Pred x) = (eval x) - 1
eval (IsZero x) = eval x == 0
eval (If e1 e2 e3) = if (eval e1) then eval e2 else eval e3 



Using GADTs, we can define the same language as: -}

data Term t where
  Zero   :: Term Int
  Succ   :: Term Int -> Term Int
  Pred   :: Term Int -> Term Int
  IsZero :: Term Int -> Term Bool
  If     :: (Term Bool) -> Term t -> Term t -> Term t
-- and now we can implement the eval function. 
eval :: Term t -> t
eval (Zero) = 0
eval (Succ x) = (eval x) + 1 
eval (Pred x) = (eval x) - 1
eval (IsZero x) = (eval x) == 0
eval (If e1 e2 e3) = if (eval e1) then eval e2 else eval e3 

{- 
Using GADTs, we can define heterogenous lists: 
-} 

-- ----------------- Heteregenous list --------------------
data HList tys where --indexed type 
  Nil  :: HList '[]
  (:>) :: h -> HList t -> HList (h ': t)

infixr 5 :>

data Elem list elt where
   EZ :: Elem (x ': xs) x
   ES :: Elem xs x -> Elem (y ': xs) x 

--get :: Int -> HList tys -> _
--get 0 (x :> xs) = x

get :: Elem tys ty -> HList tys -> ty
get EZ     (x :> _) = x
get (ES e) (x :> xs) = get e xs


{- The following example from the video tutorial by: Richard Eisenberg: https://www.youtube.com/watch?v=6snteFntvjM -} 

data Wrap a = Wrap a

data STy ty where
  SInt   :: STy Int
  SBool  :: STy Bool
  SMaybe :: STy a -> STy (Maybe a) 
  Swrap  :: STy a -> STy (Wrap a) 

deriving instance Show (STy ty)  

-- The idea here is that pattern matching on terms give you type information
-- The type signature is always required when using language extensions.
zero :: STy ty -> ty
zero SInt  = 0
zero SBool = False
zero (Swrap ty) = Wrap (zero ty)

eqSTy :: STy ty -> STy ty -> Bool
eqSTy SInt SInt = True
eqSTy SBool SBool = False
eqSTy (SMaybe a) (SMaybe b) = a `eqSTy` b 

{- ScopedTypeVariables 
foo :: a -> .. 
foo x = .. 
  where fhelper :: a 
        fhelper = ... 
In foo, the two a's can be different. 
bar :: forall a. a -> ... ---> a is lexically scoped
bas x = ... 
  where bhelper :: a 
        bhelper = ... 
In bar, the two a's have to be the same, because we added the forall  
-}

 