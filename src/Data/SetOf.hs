module SetOf where

import Control.Applicative
import Data.Monoid

import Prelude hiding(null,map)
-- Semantic function on SetOf is 'flip member': a set is
-- completely determined by knowing, for every value, whether it
-- is or isn't in the set.
--
-- 'flip member' has type: s a -> a -> Bool
--
-- The following lists the ZFC axioms (from MathWorld, see
-- http://mathworld.wolfram.com/Zermelo-FraenkelAxioms.html)
-- expressed using member:
--
-- 1. Extensionality: Two sets are equal if they have the same elements 
--       - this also somehow implies the converse (see wikipedia): 
--         two sets have the same elements if the two sets are equal.
--                    
--
-- 2. Pairing: forall x y. exists p = (pair x y): (member x p) && (member y p)
--
-- 3. Subsets: If P is a property, and Z a set, there exists a
-- subset Y of Z containing those elements of Z that satisfy
-- property P.
--     forall z p. exists (filter z p)
--       Implies: null (because taking any set z, null = filter z (const false))
--       aka. comprehension or separation
--
-- 4. Sum Set: For any X (set of sets?) there exists a set Y = union X, 
--        the union of all elements of X. It is, by the way, the set of elements of elements of X.
--    Possible Haskellism?: forall x :: SetOf (SetOf a). exists (fold union null x) (which is also join x)
--
-- 5. Power Set: For every set x there exists a powerset px, the set of all subsets of x.
--        forall x :: SetOf a. exists px :: SetOf (SetOf a). forall y :: SetOf a. (member y px) iff (subset y x)
--
-- 6. There exists an infinite set I, such that: the null (empty) set is a member of I, and for every x in I, also the
--    following is in I: the union of x with the singleton {x} (a set containing only x).
--
-- 7. Replacement: For every function F and set X, there is a set Y such that F: X -> Y. 
--                 (Meaning, if Y is the image of F on X, then Y is neccesarily a set).
--        forall x :: SetOf a. exists fx = (fmap f x),  (flip member $ fmap f x) == fmap f (flip member $ x)
--
-- 8. Regularity: Every non-empty set x contains a member y such that x and y are disjoint sets.
--        forall x. exists a. (member a x) => exists y. ( (member y x) && (not exists z. inBoth z x y ) )
--           where inBoth z x y =  member z x && member z y
--    - what to do about regularity? it only applies to sets of sets!
--    - and so does 'Sum Set'
--
-- 9. Choice: forall x :: SetOf (SetOf a). exists (choice x)  = forall m :: SetOf a, (m `member` x). exists n :: a, (n `member` m). n `member` (choice x)
--    - how to implement this?

class SetOf s a where
  -- defining property of sets, the semantic function 
  member :: a -> s a -> Bool
  
  -- existence of pair sets for any two values
  pair :: a -> b -> s (Either a b)
  
  -- existence of subsets
  filter :: s a -> (a -> Bool) -> s a

  -- the union u of two sets a, b can be defined by the semantic function, flip member, as follows:
  -- "(flip member u)" = (flip member a) || (flip member b)
  union :: s a -> s a -> s a

  -- this can only exist for countable, computably enumerable sets - not all sets!
  fold :: (a->b->b) -> b -> s a -> b
  
  -- the intersection i of two sets a, b:
  -- "(flip member i)" = (flip member a) && (flip member b)
  intersection :: s a -> s a -> s a

  singleton :: a -> s a

-- subset is implied by the existence of filter
subset :: s a -> s a -> Bool
subset subz z = subz == (filter z (flip member subz))

-- null is implied from the existence of filter. We can think of
-- null - using the semantic function (flip member) - as the
-- function that maps all values to false, namely: (const false).
null :: (SetOf s) => s a
null = filter arbSet (const false)
     -- arbSet can be any set, arbitrary choosing one that exists
     -- from the axioms:
     where arbSet = pair () () 

map :: (a->b) -> s a -> s b
map f = fold (union . singleton . f)

instance (SetOf (s a)) => Eq (s a) where
  -- equality is defined as equality of the 'flip member' functions
  sa == sb  =  (flip member $ sa) == (flip member $ sb)

instance (SetOf (s a)) => Monoid (s a) where
  --mempty :: SetOf a
  mempty = null
  
  --mappend :: (SetOf a) -> (SetOf a) -> (SetOf a)
  mappend = union
  
  --mconcat :: [SetOf a] -> (SetOf a)
  mconcat ss = foldr union null ss


instance Functor SetOf where
  --fmap :: (a->b) -> (SetOf a) -> (SetOf b)
  fmap = map
  
instance Applicative SetOf where
  --pure :: a -> SetOf a
  pure = singleton
  
  -- apply every function in functions set a on every element in set of values
 -- (<*>) :: SetOf (a->b) -> SetOf a -> SetOf b
  sa <*> sb = fold (\f sb' -> (fmap f sb) `union` sb') null sa
    
    

