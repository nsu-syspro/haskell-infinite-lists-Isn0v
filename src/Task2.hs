{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task2 where
import Data.Foldable (Foldable(toList))

-- | Infinite stream of elements
data Stream a = Stream a (Stream a)

instance Functor Stream where
  fmap f (Stream x xs) = Stream (f x) (fmap f xs)

instance Foldable Stream where
  foldMap f (Stream x xs) = f x <> foldMap f xs

instance Num a => Num (Stream a) where
  fromInteger n = fromList 0 [fromInteger n]

  negate (Stream x xs) = Stream (-x) (negate xs)
  (+) (Stream x xs) (Stream y ys) = Stream (x + y) (xs + ys)
  (*) (Stream x xs) (Stream y ys) = Stream (x * y) (fmap (* x) ys + xs * Stream y ys)
  abs (Stream x xs) = Stream (abs x) (abs xs)
  signum (Stream x xs) = Stream (signum x) (signum xs)

instance Fractional a => Fractional (Stream a) where
  fromRational = fromList 0 . repeat . fromRational
  (/) (Stream x xs) (Stream y ys) = Stream (x / y) ((xs - fmap (* (x / y)) ys) / Stream y ys)


-- | Converts given list into stream
--
-- If the list is finite then it is continued
-- with given value repeated infinitely
--
-- Usage example:
--
-- >>> fromList 0 [1,2,3]
-- [1,2,3,0,0,0,0,0,0,0]
-- >>> fromList undefined [1..]
-- [1,2,3,4,5,6,7,8,9,10]
--
fromList :: a -> [a] -> Stream a
fromList x [] = Stream x (fromList x [])
fromList x ys = foldr Stream (Stream x (fromList x [])) ys

-- | Builds stream from given seed value by applying given step function
--
-- Step function produces a pair of the next element in stream and updated seed value.
--
-- Usage example:
--
-- >>> unfold (\x -> (x, x-1)) 5
-- [5,4,3,2,1,0,-1,-2,-3,-4]
-- >>> unfold (\x -> (abs x, x-1)) 5
-- [5,4,3,2,1,0,1,2,3,4]
--
unfold :: (b -> (a, b)) -> b -> Stream a
unfold f x = Stream a (unfold f b)
  where (a, b) = f x

-- | Returns infinite stream of natural numbers (excluding zero)
--
-- First 10 natural numbers:
--
-- >>> nats
-- [1,2,3,4,5,6,7,8,9,10]
--
nats :: Stream Integer
nats = unfold (\x -> (x, x + 1)) 1

-- | Returns infinite stream of fibonacci numbers (starting with zero)
--
-- First 10 fibonacci numbers:
--
-- >>> fibs
-- [0,1,1,2,3,5,8,13,21,34]
--
fibs :: Stream Integer
fibs = unfold (\(a, b) -> (a, (b, a + b))) (0, 1)

-- | Returns infinite stream of prime numbers
--
-- First 10 prime numbers:
--
-- >>> primes
-- [2,3,5,7,11,13,17,19,23,29]
--
primes :: Stream Integer
primes = unfold sieve (fromList 0 [2..])

-- | One step of Sieve of Eratosthenes
-- (to be used with 'unfoldr')
--
-- Returns next prime number from given stream
-- and strikes out all multiples of this prime
-- from the rest of the stream
--
-- Usage example:
--
-- >>> sieve $ fromList 0 [2..]
-- (2,[3,5,7,9,11,13,15,17,19,21])
-- >>> sieve $ snd $ sieve $ fromList 0 [2..]
-- (3,[5,7,11,13,17,19,23,25,29,31])
--
sieve :: Stream Integer -> (Integer, Stream Integer)
sieve x = (p, fromList p $ filter (\y -> y `mod` p /= 0) $ toList xs)
  where Stream p xs = x 
