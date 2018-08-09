{-# LANGUAGE GADTs, DataKinds, PolyKinds, TypeOperators, TypeFamilies,
 RankNTypes, ScopedTypeVariables, ImpredicativeTypes, RebindableSyntax, NoMonomorphismRestriction #-}
module IxFree where

import Data.Type.Equality
import Prelude(Functor(..), String, (.), id, flip, error, show, (+), fromInteger)

data Proxy (a :: k) = Proxy

data HList i where
  HNil :: HList '[]
  HCons :: h -> HList t -> HList (h ': t)

type family Append (a :: [k]) (b :: [k]) :: [k] where
  Append ('[]) l = l
  Append l ('[]) = l
  Append (e ': l) l' = e ': (Append l l')

type family Tail (a :: [k]) :: [k] where
  Tail (p ': ps) = ps
  Tail '[] = '[]


leftIdentity :: Append '[] xs :~: xs
leftIdentity = Refl

rightIdentity :: Append xs '[] :~: xs
rightIdentity = Refl

associativity :: Append xs (Append ys zs) :~: Append (Append xs ys) zs
associativity = Refl





data ActionF next where
   Input :: (a -> next) ->  ActionF next
   Output :: String -> next -> ActionF next

instance Functor ActionF where
  fmap f (Input c) = Input (fmap f c)
  fmap f (Output s n) = Output s (f n)

data FreeIx f i a where
  Return :: a -> FreeIx f '[] a
  Free :: f (FreeIx f i a) -> FreeIx f i a

--data FreeIx f i a = forall j . Return a | Free (f (FreeIx f j a))

type Action i a = FreeIx ActionF i a

liftF :: Functor f => f a -> FreeIx f i a
liftF = unsafeReindex . Free . fmap Return

unsafeReindex :: (Functor f) => FreeIx f i a -> FreeIx f j a
unsafeReindex (Free f) = Free (fmap unsafeReindex f)

input :: forall a . Action '[a] a
input = liftF (Input id)

output :: String -> Action '[] ()
output s = liftF (Output s ())


bind :: Functor f => FreeIx f t a -> (a -> FreeIx f v b) -> FreeIx f (Append t v) b
bind (Return a) f = f a
bind (Free x) f   = Free (fmap (flip bind f) x)

ireturn :: a -> FreeIx f '[] a
ireturn = Return

(>>=) :: Functor f => FreeIx f t a -> (a -> FreeIx f v b) -> FreeIx f (Append t v) b
(>>=) = bind
m >> n = m >>= (\_ -> n)
return = ireturn
fail = error

prog1 = do
  a <- input
  b <- input
  output (show (a + b))
  return 5

eval :: forall params a. Action params a -> HList params -> [String]
eval (Return r) xs = []
eval (Free (Input k)) (HCons v rest) = eval ((k v) :: Action (Tail params) a)  (rest :: HList (Tail params))
eval (Free (Output v k)) l = v : eval k l





