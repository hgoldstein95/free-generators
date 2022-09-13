{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Redundant <$>" #-}
{-# HLINT ignore "Use <$" #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use >=>" #-}
{-# HLINT ignore "Use tuple-section" #-}

module FreeGen where

import Classes
  ( Choice,
    Choices,
    DerivGen,
    Derivative (..),
    Generate (..),
    HasChoices (..),
    HasIsVoid (..),
    Nullable (..),
    Pick (..),
    Weight,
    toInt,
  )
import Control.Arrow (first)
import Control.Lens (bimap, view, _2, _3)
import Control.Monad (ap, guard, join, (>=>))
import Data.HashSet (HashSet)
import Data.List (find, group)
import Test.QuickCheck (Gen)
import qualified Test.QuickCheck as Gen

type Set = HashSet

----------------------------------------------------------------------------------------------------
-- Type Definition
----------------------------------------------------------------------------------------------------

-- | The "Freer monad" interface.
data FFree f a where
  Return :: a -> FFree f a
  Bind :: f a -> (a -> FFree f b) -> FFree f b

instance Functor (FFree g) where
  fmap f (Return x) = Return (f x)
  fmap f (Bind u q) = Bind u (fmap f . q)

instance Applicative (FFree g) where
  pure = return
  (<*>) = ap

instance Monad (FFree g) where
  return = Return
  Return x >>= k = k x
  Bind u k' >>= k = Bind u (\x -> k' x >>= k)

-- | A functor representing weighted choices.
data FPick a where
  Pick :: [(Weight, Choice, FFree FPick a)] -> FPick a

-- | The type of free generators; the freer monad over the `Pick` functor.
type FreeGen = FFree FPick

-- | A pattern to capture a _definitely_ empty generator.
pattern Void :: FFree FPick a
pattern Void <- Bind (Pick []) _

-- | A smart constructor for `pick`.
instance Pick FreeGen where
  pick xs =
    case filter (\(_, _, x) -> not (isVoid x)) xs of
      xs'
        | hasDuplicates (map (\(_, c, _) -> c) xs') ->
          error "pick: Frequency cannot have duplicate choice labels"
      xs' -> Bind (Pick xs') Return

-- | A smart constructor for `void`.
void :: FreeGen a
void = Bind (Pick []) Return

----------------------------------------------------------------------------------------------------
-- Interpretations
----------------------------------------------------------------------------------------------------

-- | The language interpretation of a free generator.
language :: FreeGen a -> [Choices]
language (Return _) = [[]]
language Void = []
language (Bind (Pick xs) f) = do
  (_, c, x) <- xs
  s <- language (x >>= f)
  pure (c : s)

-- | The random generator interpretation of a free generator.
toGen :: FreeGen a -> GenBot a
toGen (Return a) = return a
toGen Void = bot
toGen (Bind (Pick xs) f) = do
  g <- frequencyBot (map (\(w, _, x) -> (w, return x)) xs)
  a <- toGen g
  toGen (f a)

-- | Partial version of `toGen`.
toGen' :: FreeGen a -> Gen a
toGen' =
  interp
    ( \case
        Pick [] -> error "toGen': Not defined on Void"
        Pick xs -> Gen.oneof (toGen' . view _3 <$> xs)
    )
  where
    interp :: Monad m => (forall b. f b -> m b) -> FFree f a -> m a
    interp _ (Return a) = pure a
    interp f (Bind x g) = f x >>= interp f . g

-- | The parser interpretation of a free generator.
toParser :: FreeGen a -> Choices -> Maybe (a, Choices)
toParser (Return a) = \s -> Just (a, s)
toParser Void = const Nothing
toParser (Bind (Pick xs) f) = \case
  [] -> Nothing
  (c : s) -> do
    (x, s') <- (`toParser` s) . view _3 =<< find ((== c) . view _2) xs
    toParser (f x) s'

-- | The choice distribution interpretation of a free generator.
toChoices :: FreeGen a -> GenBot Choices
toChoices (Return _) = pure []
toChoices Void = bot
toChoices (Bind (Pick xs) f) = do
  (c, g) <- frequencyBot (map (\(w, c, x) -> (w, return (c, x))) xs)
  s <- toChoices (g >>= f)
  pure (c : s)

----------------------------------------------------------------------------------------------------
-- Derivatives
----------------------------------------------------------------------------------------------------

instance Nullable (FFree g) where
  nu (Return a) = [a]
  nu _ = []

instance Derivative (FreeGen a) where
  delta _ (Return _) = void
  delta _ Void = void
  delta c (Bind (Pick xs) f) =
    case find ((== c) . view _2) xs of
      Just (_, _, x) -> x >>= f
      Nothing -> void

instance Generate FreeGen where
  gen Void = pure Nothing
  -- NOTE: This is slightly different from the implementation in the paper. In particular,
  -- we found that explicitly dealing with `Maybe` (rather than just writing partial generators)
  -- was slightly more expensive.
  gen g = Just <$> toGen' g -- runGenBot (toGen g)

instance DerivGen FreeGen

----------------------------------------------------------------------------------------------------
-- Other Definitions
----------------------------------------------------------------------------------------------------

instance HasChoices (FreeGen a) where
  getChoices (Return _) = []
  getChoices (Bind (Pick xs) _) = map (view _2) xs

instance HasIsVoid (FreeGen a) where
  isVoid Void = True
  isVoid _ = False

hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates = any ((> 1) . length) . group

filterG :: (a -> Bool) -> Gen (Maybe a) -> Gen (Maybe a)
filterG valid g = ((\a -> if valid a then Just a else Nothing) =<<) <$> g

newtype GenBot a = GenBot {runGenBot :: Gen (Maybe a)}

instance Functor GenBot where
  fmap f = (>>= return . f)

instance Applicative GenBot where
  pure = return
  (<*>) = ap

instance Monad GenBot where
  return = GenBot . pure . Just
  x >>= f = GenBot $ do
    a <- runGenBot x
    case a of
      Just a' -> runGenBot (f a')
      Nothing -> return Nothing

bot :: GenBot a
bot = GenBot (return Nothing)

frequencyBot :: [(Weight, GenBot a)] -> GenBot a
frequencyBot xs = GenBot (Gen.frequency (map (bimap (fromInteger . toInt) runGenBot) xs))

newtype Parser a = Parser {parse :: Choices -> Maybe (a, Choices)}

instance Functor Parser where
  fmap f x = Parser (fmap (first f) . parse x)

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  return a = Parser (const (Just (a, [])))
  x >>= f = Parser $ \s -> do
    (a, s') <- parse x s
    parse (f a) s'

----------------------------------------------------------------------------------------------------
-- Informal Proofs
----------------------------------------------------------------------------------------------------

(===) :: a -> a -> a
(===) = error "not implemented"

lemma1 :: String -> (a -> b) -> FreeGen a -> FreeGen b
lemma1 c f x =
  delta c (f <$> x)
    -- Monad Laws
    === delta c (x >>= pure . f)
    -- Definition of (>>=)
    === delta
      c
      ( case x of
          Return x' -> pure (f x')
          Bind u k -> Bind u (k >=> pure . f)
      )
    -- Commutativity of Case
    === ( case x of
            Return x' -> delta c (pure (f x'))
            Bind u k -> delta c (Bind u (k >=> pure . f))
        )
    -- Monad Laws
    === ( case x of
            Return x' -> f <$> delta c (pure x')
            Bind u k -> f <$> delta c (Bind u (k >=> pure))
        )
    -- Commutativity of Case
    === ( f <$> case x of
            Return a -> delta c (pure a)
            Bind u k -> delta c (Bind u (k >=> pure))
        )
    -- Monad Laws
    === ( f <$> case x of
            Return a -> delta c (Return a)
            Bind u k -> delta c (Bind u k)
        )
    === (f <$> delta c x)

theorem1 :: String -> FreeGen (a -> b) -> FreeGen a -> FreeGen b
theorem1 c f x =
  delta c (f <*> x)
    -- Monad Laws
    === delta c (f >>= (<$> x))
    -- Definition of (>>=)
    === delta
      c
      ( case f of
          Return f' -> f' <$> x
          Bind u k -> Bind u (k >=> (<$> x))
      )
    -- Commutativity of Case
    === ( case f of
            Return f' -> delta c (f' <$> x)
            Bind u k -> delta c (Bind u (k >=> (<$> x)))
        )
    -- lemma1
    === ( case f of
            Return f' -> f' <$> delta c x
            Bind u k -> delta c (Bind u (k >=> (<$> x)))
        )
    -- Definition of delta
    === ( case f of
            Return f' -> f' <$> delta c x
            Bind (Pick xs) k ->
              case find ((== c) . view _2) xs of
                Just (_, _, y) -> y >>= (k >=> (<$> x))
                Nothing -> void
        )
    -- Monad Laws
    === ( case f of
            Return f' -> f' <$> delta c x
            Bind (Pick xs) k ->
              case find ((== c) . view _2) xs of
                Just (_, _, y) -> (y >>= k) >>= (<$> x)
                Nothing -> void
        )
    -- Definition of (===)
    === ( case f of
            Return f' -> f' <$> delta c x
            Bind (Pick xs) k ->
              case find ((== c) . view _2) xs of
                Just (_, _, y) -> (y >>= k) >>= (<$> x)
                Nothing -> void >>= (<$> x)
        )
    -- Commutativity of Case
    === ( case f of
            Return f' -> f' <$> delta c x
            Bind (Pick xs) k ->
              ( case find ((== c) . view _2) xs of
                  Just (_, _, y) -> y >>= k
                  Nothing -> void
              )
                >>= (<$> x)
        )
    -- Definition of delta
    === ( case f of
            Return f' -> f' <$> delta c x
            Bind _ _ -> delta c f >>= (<$> x)
        )
    -- Monad Laws
    === ( case f of
            Return f' -> f' <$> delta c x
            Bind _ _ -> delta c f <*> x
        )
    -- Nested Cases
    === ( case (case f of Return f' -> [f']; _ -> []) of
            [f'] -> f' <$> delta c x
            _ -> delta c f <*> x
        )
    -- Definition of nu
    === ( case nu f of
            [f'] -> f' <$> delta c x
            _ -> delta c f <*> x
        )

delta' :: Choice -> [Choices] -> [Choices]
delta' c xs = do
  (c' : cs) <- xs
  guard (c == c')
  pure cs

theorem2 :: Choice -> FreeGen a -> [Choices]
theorem2 c x =
  language (delta c x)
    -- Definition of delta
    === language
      ( case x of
          Return _ -> void
          Bind (Pick xs) k ->
            ( case find ((== c) . view _2) xs of
                Just (_, _, y) -> y >>= k
                Nothing -> void
            )
      )
    -- Definition of language
    === ( case x of
            Return _ -> []
            Bind (Pick xs) k ->
              ( case find ((== c) . view _2) xs of
                  Just (_, _, y) -> language (y >>= k)
                  Nothing -> []
              )
        )
    -- List Monad Rewrites
    === ( case x of
            Return _ -> []
            Bind (Pick xs) k -> do
              (_, c', x') <- xs
              cs <- language (x' >>= k)
              guard (c == c')
              pure cs
        )
    -- Refactor Case
    === ( do
            (c' : cs) <- case x of
              Return _ -> [[]]
              Bind (Pick xs) k -> do
                (_, c', x') <- xs
                s <- language (x' >>= k)
                pure (c' : s)
            guard (c == c')
            pure cs
        )
    -- Definition of language
    === ( do
            (c' : cs) <- language x
            guard (c == c')
            pure cs
        )
    -- Definition of delta
    === delta' c (language x)

squash :: GenBot (Maybe a) -> GenBot a
squash (GenBot x) = GenBot (join <$> x)

theorem3 :: FreeGen a -> GenBot (a, Choices)
theorem3 x =
  squash (toParser x <$> toChoices x)
    -- Definition of toParser and toChoices
    === squash
      ( case x of
          Return a -> (\s -> Just (a, s)) <$> pure []
          Void -> const Nothing <$> bot
          Bind (Pick xs) k ->
            ( \case
                [] -> Nothing
                (c : s) -> do
                  (y, s') <- (`toParser` s) . view _3 =<< find ((== c) . view _2) xs
                  toParser (k y) s'
            )
              <$> ( do
                      (c, g) <- frequencyBot (map (\(w, c, y) -> (w, return (c, y))) xs)
                      s <- toChoices (g >>= k)
                      pure (c : s)
                  )
      )
    -- Simplification
    === squash
      ( case x of
          Return a -> (\b -> Just (b, [])) <$> pure a
          Void -> (\b -> Just (b, [])) <$> bot
          Bind (Pick xs) k -> do
            g <- frequencyBot (map (\(w, _, y) -> (w, return y)) xs)
            s <- toChoices (g >>= k)
            pure $ do
              (y, s') <- toParser g s
              toParser (k y) s'
      )
    -- Simplification
    === squash
      ( case x of
          Return a -> (\b -> Just (b, [])) <$> pure a
          Void -> (\b -> Just (b, [])) <$> bot
          Bind (Pick xs) k -> do
            g <- frequencyBot (map (\(w, _, y) -> (w, return y)) xs)
            s <- toChoices (g >>= k)
            pure $ toParser (g >>= k) s
      )
    -- Simplification
    === squash
      ( case x of
          Return a -> (\b -> Just (b, [])) <$> pure a
          Void -> (\b -> Just (b, [])) <$> bot
          Bind (Pick xs) k -> do
            g <- frequencyBot (map (\(w, _, y) -> (w, return y)) xs)
            toParser (g >>= k) <$> toChoices (g >>= k)
      )
    -- lemma2
    === ( case x of
            Return a -> (\b -> (b, [])) <$> pure a
            Void -> (\b -> (b, [])) <$> bot
            Bind (Pick xs) k -> do
              g <- frequencyBot (map (\(w, _, y) -> (w, return y)) xs)
              squash (toParser g <$> toChoices g)
                >>= \(a, []) -> squash (toParser (k a) <$> toChoices (k a))
        )
    -- IH
    === ( case x of
            Return a -> (\b -> (b, [])) <$> pure a
            Void -> (\b -> (b, [])) <$> error ""
            Bind (Pick xs) k -> do
              g <- frequencyBot (map (\(w, _, y) -> (w, return y)) xs)
              ((\b -> (b, [])) <$> toGen g) >>= \(a, []) -> (\b -> (b, [])) <$> toGen (k a)
        )
    -- Simplification
    === ( case x of
            Return a -> (\b -> (b, [])) <$> pure a
            Void -> (\b -> (b, [])) <$> bot
            Bind (Pick xs) k -> do
              g <- frequencyBot (map (\(w, _, y) -> (w, return y)) xs)
              a <- toGen g
              (\b -> (b, [])) <$> toGen (k a)
        )
    -- Simplification
    === ( (\b -> (b, [])) <$> case x of
            Return a -> return a
            Void -> bot
            Bind (Pick xs) k -> do
              g <- frequencyBot (map (\(w, _, y) -> (w, return y)) xs)
              a <- toGen g
              toGen (k a)
        )
    -- Definition of toGen
    === ((\a -> (a, [])) <$> toGen x)

lemma2 :: FreeGen a -> (a -> FreeGen b) -> GenBot (b, Choices)
lemma2 (Return a) f =
  squash (toParser (Return a >>= f) <$> toChoices (Return a >>= f))
    -- Monad Laws
    === squash (toParser (f a) <$> toChoices (f a))
    -- Eta expand with case
    === squash ((\(b, []) -> toParser (f b) <$> toChoices (f b)) (a, []))
    -- Monad Laws, Definitions of toParser and toChoices
    === ( squash (toParser (Return a) <$> toChoices (Return a))
            >>= (\(b, []) -> squash (toParser (f b) <$> toChoices (f b)))
        )
lemma2 (Bind (Pick xs) k) f =
  squash (toParser (Bind (Pick xs) k >>= f) <$> toChoices (Bind (Pick xs) k >>= f))
    -- Definition of (>>=)
    === squash (toParser (Bind (Pick xs) (k >=> f)) <$> toChoices (Bind (Pick xs) (k >=> f)))
    -- Definition of toParser and toChoices
    === squash
      ( ( \case
            [] -> Nothing
            (c : s) -> do
              (x, s') <- (`toParser` s) . view _3 =<< find ((== c) . view _2) xs
              toParser ((k >=> f) x) s'
        )
          <$> ( do
                  (c, g) <- frequencyBot (map (\(w, c, y) -> (w, return (c, y))) xs)
                  s <- toChoices (g >>= (k >=> f))
                  pure (c : s)
              )
      )
    -- Simplification
    === squash
      ( do
          (_, g) <- frequencyBot (map (\(w, c, y) -> (w, return (c, y))) xs)
          toParser (g >>= (k >=> f)) <$> toChoices (g >>= (k >=> f))
      )
    -- Monad Laws
    === ( do
            (_, g) <- frequencyBot (map (\(w, c, y) -> (w, return (c, y))) xs)
            squash (toParser ((g >>= k) >>= f) <$> toChoices ((g >>= k) >>= f))
        )
    -- IH
    === ( do
            (_, g) <- frequencyBot (map (\(w, c, y) -> (w, return (c, y))) xs)
            squash (toParser (g >>= k) <$> toChoices (g >>= k))
              >>= (\(a, []) -> squash (toParser (f a) <$> toChoices (f a)))
        )
    -- Expand
    === ( squash
            ( ( \case
                  [] -> Nothing
                  (c : s) -> do
                    (x, s') <- (`toParser` s) . view _3 =<< find ((== c) . view _2) xs
                    toParser (k x) s'
              )
                <$> ( do
                        (c, g) <- frequencyBot (map (\(w, c, y) -> (w, return (c, y))) xs)
                        s <- toChoices (g >>= k)
                        pure (c : s)
                    )
            )
            >>= (\(a, []) -> squash (toParser (f a) <$> toChoices (f a)))
        )
    -- Definition of toParser and toChoices
    === ( squash (toParser (Bind (Pick xs) k) <$> toChoices (Bind (Pick xs) k))
            >>= (\(a, []) -> squash (toParser (f a) <$> toChoices (f a)))
        )
