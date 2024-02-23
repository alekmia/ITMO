module HW3.T2
  ( distOption
  , wrapOption
  , distPair
  , wrapPair
  , distQuad
  , wrapQuad
  , distAnnotated
  , wrapAnnotated
  , distExcept
  , wrapExcept
  , distPrioritised
  , wrapPrioritised
  , distStream
  , wrapStream
  , distList
  , wrapList
  , distFun
  , wrapFun
  ) where

import HW3.T1

distOption :: (Option a, Option b) -> Option (a, b)
distOption (Some a, Some b) = Some (a, b)
distOption _ = None

wrapOption :: a -> Option a
wrapOption = Some

distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P a b, P c d) = P (a, c) (b, d)

wrapPair :: a -> Pair a
wrapPair a = P a a

distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q a b c d, Q w x y z) = Q (a, w) (b, x) (c, y) (d, z)

wrapQuad :: a -> Quad a
wrapQuad a = Q a a a a

distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (a1 :# e1, a2 :# e2) = (a1, a2) :# (e1 <> e2)

wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated a = a :# mempty

distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Success a, Success b) = Success (a, b)
distExcept (Error a, _) = Error a
distExcept (_, Error b) = Error b

wrapExcept :: a -> Except e a
wrapExcept = Success

distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (Low a, Low b) = Low (a, b)
distPrioritised (Low a, Medium b) = Medium (a, b)
distPrioritised (Low a, High b) = High (a, b)
distPrioritised (Medium a, Low b) = Medium (a, b)
distPrioritised (Medium a, Medium b) = Medium (a, b)
distPrioritised (Medium a, High b) = High (a, b)
distPrioritised (High a, Low b) = High (a, b)
distPrioritised (High a, Medium b) = High (a, b)
distPrioritised (High a, High b) = High (a, b)

wrapPrioritised :: a -> Prioritised a
wrapPrioritised = Low

distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (a :> b, c :> d) = (a, c) :> distStream (b, d)

wrapStream :: a -> Stream a
wrapStream a = a :> wrapStream a 

addLists :: List a -> List a -> List a
addLists (a :. b) c = a :. addLists b c
addLists Nil c = c

distListImpl :: a -> List b -> List (a, b)
distListImpl _ Nil = Nil
distListImpl a (b :. c) = addLists ((a, b) :. Nil) (distListImpl a c)

distList :: (List a, List b) -> List (a, b)
distList (Nil, _) = Nil
distList (_, Nil) = Nil
distList (a :. b, c) = addLists (distListImpl a c) (distList (b, c)) 

wrapList :: a -> List a
wrapList a = a :. Nil

distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F f, F g) = F (\i -> (f i, g i))

wrapFun :: a -> Fun i a
wrapFun a = F (const a)
