{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Classes where

import Control.Arrow (first, second)
import Control.Monad (forM, replicateM)
import Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import Data.Hashable (Hashable)
import Data.Maybe (catMaybes)
import Test.QuickCheck (Gen)
import qualified Test.QuickCheck as Gen

newtype Weight = Weight {toInt :: Integer} deriving (Eq, Ord)

instance Num Weight where
  Weight x + Weight y = Weight (x + y)
  Weight x * Weight y = Weight (x * y)
  abs = id
  signum = id
  fromInteger i = if i <= 0 then error "invalid weight" else Weight i
  negate = error "cannot negate weight"

instance Show Weight where
  show = show . toInt

type Choice = String

type Choices = [Choice]

class Pick f where
  pick :: [(Weight, Choice, f a)] -> f a

class HasChoices a where
  getChoices :: a -> [Choice]

class HasIsVoid a where
  isVoid :: a -> Bool

class Nullable f where
  nu :: f a -> [a]

class Derivative g where
  delta :: Choice -> g -> g

class Generate f where
  gen :: f a -> Gen (Maybe a)

class
  ( Applicative g,
    Pick g,
    Nullable g,
    forall a. Derivative (g a),
    forall a. HasIsVoid (g a),
    forall a. HasChoices (g a),
    Generate g
  ) =>
  DerivGen g

---

type Set = HashSet

generate ::
  ( DerivGen g,
    Hashable a,
    Eq a
  ) =>
  g a ->
  (a -> Bool) ->
  Int ->
  IO [a]
generate initG valid n = Set.toList <$> aux initG Set.empty
  where
    aux g results
      | not (null (nu g)) =
        -- We found a valid choice sequence
        return (Set.unions [Set.fromList (filter valid (nu g)), results])
      | isVoid g = aux initG results
      | otherwise = do
        -- We haven't found a valid sequence yet
        let grad = map (`delta` g) (getChoices g) -- Compute the gradient
        (fgs, v) <- -- Sample from each generator in the gradient
          sampleFitness valid n grad
        -- If sampling failed completely, just weight the derivatives uniformly
        let gs = if all ((== 0) . fst) fgs then map (first (const 1)) fgs else fgs
        -- Choose a derivative based on the weights
        g' <- Gen.generate (Gen.frequency (second pure <$> gs))
        aux g' (Set.union v results) -- Keep searching

sampleFitness ::
  ( Eq a,
    Hashable a,
    HasIsVoid (g a),
    Generate g
  ) =>
  (a -> Bool) ->
  Int ->
  [g a] ->
  IO ([(Int, g a)], HashSet a)
sampleFitness p samples grad =
  second Set.unions . unzip
    <$> forM
      grad
      ( \(dg :: g a) -> do
          xs <-
            if isVoid dg
              then pure []
              else catMaybes <$> replicateM samples (Gen.generate (gen dg))
          let v = Set.fromList (filter p xs)
          let f = Set.size v
          return ((f, dg), v)
      )
