{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
module Data.MetricSpace where


import Data.Complex

class (Eq m, Floating a) => MetricSpace m a where
  distance :: m -> m -> a
  
-- The distance function must satisfy the following:
--   1. d(x, y) = 0   if and only if   x = y
--   2. d(x, z) <= d(x, y) + d(z, y).

  

instance (Floating a) => MetricSpace Int     a  where distance x y = fromIntegral . abs $ (y-x)
instance (Floating a) => MetricSpace Integer a  where distance x y = fromIntegral . abs $ (y-x)
                                                      
-- Default instances use the same floating point type as the underlying type.
instance MetricSpace Float Float   where distance x y = abs (y-x)
instance MetricSpace Double Double where distance x y = abs (y-x)
instance (RealFrac a, RealFloat a) => MetricSpace (Complex a) a
    where distance x y = realPart . abs $ (y-x)
                                   
