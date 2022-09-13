{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

module Examples where

import Classes (Pick (pick))
import Control.Arrow (first, second)
import Control.Monad (guard)
import Control.Monad.Reader (ReaderT (runReaderT), ask, local)
import Control.Monad.Trans (lift)
import Data.Hashable (Hashable (..))
import Data.List (nub, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import FreeGen (FreeGen)
import GHC.Generics (Generic)

----------------------------------------------------------------------------------------------------
-- SETUP
----------------------------------------------------------------------------------------------------

class HasStats a where
  sizeFn :: a -> Int
  nodeFreqs :: a -> Map (Class, String) Int
  ungenerate :: a -> String
  subtrees :: a -> Set a

newtype Class = Class String deriving (Ord, Eq, Show)

unit :: Class -> String -> Map (Class, String) Int
unit c = (`Map.singleton` 1) . (c,)

mergeFreqs :: Ord k => [Map k Int] -> Map k Int
mergeFreqs = Map.unionsWith (+)

normClasses :: Map (Class, String) Int -> Map (Class, String) Double
normClasses m = Map.mapWithKey (\(c, _) a -> fromIntegral a / fromIntegral (classSum c)) m
  where
    classSum c = sum . Map.elems . Map.filterWithKey (\(c', _) _ -> c == c') $ m

----------------------------------------------------------------------------------------------------
-- Binary Search Trees
----------------------------------------------------------------------------------------------------

data Tree a = Node a (Tree a) (Tree a) | Leaf
  deriving (Eq, Ord, Show, Read, Generic, Hashable)

isBST :: Ord a => Tree a -> Bool
isBST Leaf = True
isBST (Node x l r) =
  all (< x) (treeToList l)
    && all (> x) (treeToList r)
    && isBST l
    && isBST r

treeToList :: Tree a -> [a]
treeToList t = aux t []
  where
    aux Leaf = id
    aux (Node x l r) = aux l . (x :) . aux r

genTree :: (Applicative g, Pick g) => g (Tree Int)
genTree = aux (5 :: Int)
  where
    aux 0 = pure Leaf
    aux n =
      pick
        [ ( 1,
            "l",
            pure Leaf
          ),
          ( 1,
            "n",
            Node <$> genInt <*> aux (n - 1) <*> aux (n - 1)
          )
        ]
    genInt = pick [(1, show n, pure n) | n <- [0 .. 9]]

genBST :: (Int, Int) -> FreeGen (Tree Int)
genBST (lo, hi) | lo > hi = pure Leaf
genBST (lo, hi) = do
  c <- pick [(1, "l", pure False), (1, "n", pure True)]
  if c
    then pure Leaf
    else do
      x <- pick [(1, show n, pure n) | n <- [lo .. hi]]
      l <- genBST (lo, x - 1)
      r <- genBST (x + 1, hi)
      pure (Node x l r)

instance HasStats (Tree Int) where
  sizeFn Leaf = 0
  sizeFn (Node _ l r) = 1 + sizeFn l + sizeFn r

  nodeFreqs (Node x l r) =
    Map.unionsWith
      (+)
      [ unit (Class "value") (show x),
        unit (Class "structure") "Node",
        nodeFreqs l,
        nodeFreqs r
      ]
  nodeFreqs Leaf = unit (Class "structure") "Leaf"

  ungenerate (Node x l r) = 'n' : (['0' .. '9'] !! x) : ungenerate l ++ ungenerate r
  ungenerate Leaf = "l"

  subtrees Leaf = Set.singleton Leaf
  subtrees n@(Node _ l r) = Set.unions [Set.singleton n, subtrees l, subtrees r]

sliftA2 :: (Ord a, Ord b, Ord c) => (a -> b -> c) -> Set a -> Set b -> Set c
sliftA2 f a b = Set.map (uncurry f) (Set.cartesianProduct a b)

splits :: Int -> [(Int, Int)]
splits = nub . sort . aux
  where
    aux 0 = [(0, 0)]
    aux k =
      let ss = aux (k - 1)
       in map (first (+ 1)) ss ++ map (second (+ 1)) ss

----------------------------------------------------------------------------------------------------
-- Sorted Lists
----------------------------------------------------------------------------------------------------

genList :: (Applicative g, Pick g) => g [Int]
genList = aux (20 :: Int)
  where
    aux 0 = pure []
    aux n =
      pick
        [ (1, "n", pure []),
          (1, "c", (:) <$> genInt <*> aux (n - 1))
        ]
    genInt = pick [(1, show n, pure n) | n <- [0 .. 9]]

isSorted :: [Int] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x : y : xs) = x <= y && isSorted (y : xs)

instance HasStats [Int] where
  sizeFn = length

  nodeFreqs (x : xs) =
    Map.unionsWith
      (+)
      [ unit (Class "value") (show x),
        unit (Class "structure") "Cons",
        nodeFreqs xs
      ]
  nodeFreqs [] = unit (Class "structure") "Nil"

  ungenerate = map (['0' .. '9'] !!)

  subtrees [] = Set.singleton []
  subtrees l@(_ : xs) = Set.insert l (subtrees xs)

----------------------------------------------------------------------------------------------------
-- Trees with Leaves That Multiply to Three
----------------------------------------------------------------------------------------------------

data T = L Int | N T T
  deriving (Eq, Ord, Show, Read, Generic, Hashable)

foldT :: (Int -> a) -> (a -> a -> a) -> T -> a
foldT b _ (L x) = b x
foldT b n (N l r) = n (foldT b n l) (foldT b n r)

sumLeavesMul3 :: T -> Bool
sumLeavesMul3 !t = foldT (`mod` 3) (\x y -> (x + y) `mod` 3) t == 0

toList :: T -> [Int]
toList = foldT (: []) (++)

genT :: (Applicative g, Pick g) => g T
genT = aux (5 :: Int)
  where
    aux 0 = L <$> genInt
    aux n =
      pick
        [ ( 1,
            "l",
            L <$> genInt
          ),
          ( 1,
            "n",
            N <$> aux (n - 1) <*> aux (n - 1)
          )
        ]
    genInt = pick [(1, show n, pure n) | n <- [0 .. 9]]

instance HasStats T where
  sizeFn = length . toList
  nodeFreqs (N l r) =
    Map.unionsWith
      (+)
      [ unit (Class "structure") "Node",
        nodeFreqs l,
        nodeFreqs r
      ]
  nodeFreqs (L x) =
    Map.unionsWith
      (+)
      [ unit (Class "value") (show x),
        unit (Class "structure") "Leaf"
      ]

  ungenerate (L x) = "l" ++ show x
  ungenerate (N l r) = "n" ++ ungenerate l ++ ungenerate r

  subtrees t@(L _) = Set.singleton t
  subtrees t@(N l r) = Set.unions [Set.singleton t, subtrees l, subtrees r]

----------------------------------------------------------------------------------------------------
-- AVL Trees
----------------------------------------------------------------------------------------------------

data AVL a = AVLNode a (AVL a) (AVL a) Int | AVLLeaf
  deriving (Eq, Ord, Show, Read, Generic, Hashable)

getHeight :: AVL a -> Int
getHeight AVLLeaf = 0
getHeight (AVLNode _ _ _ h) = h

avlNode :: a -> AVL a -> AVL a -> AVL a
avlNode x l r = AVLNode x l r (1 + max (getHeight l) (getHeight r))

isAVL :: Ord a => AVL a -> Bool
isAVL tree = avlIsBST tree && avlHeightInv tree && avlBalanceInv tree
  where
    avlIsBST AVLLeaf = True
    avlIsBST (AVLNode x l r _) =
      all (< x) (avlToList l)
        && all (> x) (avlToList r)
        && avlIsBST l
        && avlIsBST r

    avlHeightInv AVLLeaf = True
    avlHeightInv (AVLNode _ l r h) =
      h == 1 + max (getHeight l) (getHeight r)
        && avlHeightInv l
        && avlHeightInv r

    avlBalanceInv AVLLeaf = True
    avlBalanceInv (AVLNode _ l r _) =
      abs (getHeight l - getHeight r) <= 1
        && avlBalanceInv l
        && avlBalanceInv r

avlToList :: AVL a -> [a]
avlToList t = aux t []
  where
    aux AVLLeaf = id
    aux (AVLNode x l r _) = aux l . (x :) . aux r

genAVLSkeleton :: forall g. (Applicative g, Pick g) => g (AVL Int)
genAVLSkeleton = aux (5 :: Int)
  where
    aux :: Int -> g (AVL Int)
    aux 0 = pure AVLLeaf
    aux n =
      pick
        [ ( 1,
            "l",
            pure AVLLeaf
          ),
          ( 1,
            "n",
            do
              h <- genInt
              x <- genInt
              l <- aux (n - 1)
              r <- aux (n - 1)
              pure (AVLNode x l r h)
          )
        ]
    genInt = pick [(1, show n, pure n) | n <- [0 .. 9]]

genAVLSkeletonCache :: forall g. (Applicative g, Pick g) => g (AVL Int)
genAVLSkeletonCache = aux (5 :: Int)
  where
    aux :: Int -> g (AVL Int)
    aux 0 = pure AVLLeaf
    aux n =
      pick
        [ ( 1,
            "l",
            pure AVLLeaf
          ),
          ( 1,
            "n",
            do
              x <- genInt
              l <- aux (n - 1)
              r <- aux (n - 1)
              pure (AVLNode x l r (1 + max (getHeight l) (getHeight r)))
          )
        ]
    genInt = pick [(1, show n, pure n) | n <- [0 .. 9]]

instance HasStats (AVL Int) where
  sizeFn AVLLeaf = 0
  sizeFn (AVLNode _ l r _) = 1 + sizeFn l + sizeFn r

  nodeFreqs (AVLNode x l r _) =
    Map.unionsWith
      (+)
      [ unit (Class "value") (show x),
        unit (Class "structure") "Node",
        nodeFreqs l,
        nodeFreqs r
      ]
  nodeFreqs AVLLeaf = unit (Class "structure") "Leaf"

  ungenerate (AVLNode x l r h) =
    'n' : (['a', 'b' ..] !! x) : ungenerate l ++ ungenerate r ++ [['0' .. '9'] !! h]
  ungenerate AVLLeaf = "l"

  subtrees AVLLeaf = Set.singleton AVLLeaf
  subtrees n@(AVLNode _ l r _) = Set.unions [Set.singleton n, subtrees l, subtrees r]

----------------------------------------------------------------------------------------------------
-- Red-Black Trees
----------------------------------------------------------------------------------------------------

newtype RBT a = RBT (RBTree a)
  deriving (Eq, Ord, Show, Read, Generic, Hashable)

data Color = Red | Black
  deriving (Eq, Ord, Show, Read, Generic, Hashable)

data RBTree a = RBNode Color a (RBTree a) (RBTree a) | RBLeaf
  deriving (Eq, Ord, Show, Read, Generic, Hashable)

getColor :: RBTree a -> Color
getColor RBLeaf = Black
getColor (RBNode c _ _ _) = c

blackHeight :: RBTree a -> Int
blackHeight RBLeaf = 1
blackHeight (RBNode c _ l r) = (if c == Black then 1 else 0) + max (blackHeight l) (blackHeight r)

isRBT :: Ord a => RBT a -> Bool
isRBT (RBT tree) = rbtIsBST tree && rnbcInv tree && blackHeightInv tree
  where
    rbtIsBST RBLeaf = True
    rbtIsBST (RBNode _ x l r) =
      all (< x) (rbtToList l)
        && all (> x) (rbtToList r)
        && rbtIsBST l
        && rbtIsBST r

    rnbcInv RBLeaf = True
    rnbcInv (RBNode c _ l r) =
      (c == Black || getColor l == Black && getColor r == Black) && rnbcInv l && rnbcInv r

    blackHeightInv RBLeaf = True
    blackHeightInv (RBNode _ _ l r) =
      (blackHeight l == blackHeight r) && blackHeightInv l && blackHeightInv r

rbtToList :: RBTree a -> [a]
rbtToList t = aux t []
  where
    aux RBLeaf = id
    aux (RBNode _ x l r) = aux l . (x :) . aux r

genRBTSkeleton :: forall g. (Applicative g, Pick g) => g (RBT Int)
genRBTSkeleton = RBT <$> aux (5 :: Int)
  where
    aux :: Int -> g (RBTree Int)
    aux 0 = pure RBLeaf
    aux n =
      pick
        [ ( 1,
            "l",
            pure RBLeaf
          ),
          ( 1,
            "n",
            RBNode
              <$> pick [(1, "r", pure Red), (1, "b", pure Black)]
              <*> genInt
              <*> aux (n - 1)
              <*> aux (n - 1)
          )
        ]
    genInt = pick [(1, show n, pure n) | n <- [0 .. 9]]

instance HasStats (RBT Int) where
  sizeFn (RBT t) = aux t
    where
      aux RBLeaf = 0
      aux (RBNode _ _ l r) = 1 + aux l + aux r

  nodeFreqs (RBT t) = aux t
    where
      aux (RBNode _ x l r) =
        Map.unionsWith
          (+)
          [ unit (Class "value") (show x),
            unit (Class "structure") "Node",
            aux l,
            aux r
          ]
      aux RBLeaf = unit (Class "structure") "Leaf"

  ungenerate (RBT t) = aux t
    where
      aux (RBNode c x l r) =
        (case c of Red -> 'r'; Black -> 'b') : 'n' : (['a', 'b' ..] !! x) : aux l ++ aux r
      aux RBLeaf = "l"

  subtrees = undefined

----------------------------------------------------------------------------------------------------
-- STLC Expressions
----------------------------------------------------------------------------------------------------

data Type = TInt | Fun Type Type
  deriving (Show, Eq, Ord, Generic, Hashable)

data Expr = Int Int | Plus Expr Expr | Lam Type Expr | Var Int | App Expr Expr
  deriving (Show, Eq, Ord, Generic, Hashable)

typeOf :: Expr -> Maybe Type
typeOf expr = runReaderT (aux expr) []
  where
    aux :: Expr -> ReaderT [Type] Maybe Type
    aux (Int _) = return TInt
    aux (Plus e1 e2) = do
      TInt <- aux e1
      TInt <- aux e2
      return TInt
    aux (Lam t e) = do
      t' <- local (t :) (aux e)
      return (Fun t t')
    aux (App e1 e2) = do
      (Fun t1 t2) <- aux e1
      t1' <- aux e2
      guard (t1 == t1')
      return t2
    aux (Var n) = do
      ctx <- ask
      if length ctx <= n then lift Nothing else return (ctx !! n)

hasType :: Expr -> Bool
hasType = isJust . typeOf

genExpr :: (Applicative g, Pick g) => g Expr
genExpr = aux (5 :: Int)
  where
    aux 0 = pick [(1, "i", Int <$> genInt), (1, "v", Var <$> genVar)]
    aux n =
      pick
        [ (1, "i", pick [(1, show i, pure (Int i)) | i <- [0 .. 3]]),
          (1, "+", Plus <$> aux (n - 1) <*> aux (n - 1)),
          (1, "λ", Lam <$> genType (2 :: Int) <*> aux (n - 1)),
          (1, "@", App <$> aux (n - 1) <*> aux (n - 1)),
          (1, "v", Var <$> genVar)
        ]

    genInt = pick [(1, show n, pure n) | n <- [0 .. 3]]
    genVar = pick [(1, ["x", "y", "z"] !! v, pure v) | v <- [0, 1, 2]]
    genType 0 = pure TInt
    genType n =
      pick
        [ (1, "ℕ", pure TInt),
          (1, "→", Fun <$> genType (n - 1) <*> genType (n - 1))
        ]

genScopedExpr :: (Applicative g, Pick g) => g Expr
genScopedExpr = aux (5 :: Int) (-1)
  where
    aux 0 ctx = pick [(1, "i", Int <$> genInt), (1, "v", Var <$> genVar ctx)]
    aux n ctx =
      pick
        [ (1, "i", pick [(1, show i, pure (Int i)) | i <- [0 .. 3]]),
          (1, "+", Plus <$> aux (n - 1) ctx <*> aux (n - 1) ctx),
          (1, "λ", Lam <$> genType (2 :: Int) <*> aux (n - 1) (ctx + 1)),
          (1, "@", App <$> aux (n - 1) ctx <*> aux (n - 1) ctx),
          (1, "v", Var <$> genVar ctx)
        ]

    genInt = pick [(1, show n, pure n) | n <- [0 .. 3]]
    genVar ctx = pick [(1, ["x", "y", "z"] !! v, pure v) | v <- [0 .. ctx]]
    genType 0 = pure TInt
    genType n =
      pick
        [ (1, "ℕ", pure TInt),
          (1, "→", Fun <$> genType (n - 1) <*> genType (n - 1))
        ]

instance HasStats Expr where
  sizeFn (Int _) = 1
  sizeFn (Plus e1 e2) = 1 + sizeFn e1 + sizeFn e2
  sizeFn (Lam _ e) = 1 + sizeFn e
  sizeFn (Var _) = 1
  sizeFn (App e1 e2) = 1 + sizeFn e1 + sizeFn e2
  nodeFreqs (Int i) = unit (Class "structure") ("Lit_" ++ show i)
  nodeFreqs (Plus x y) =
    mergeFreqs
      [unit (Class "structure") "Plus", nodeFreqs x, nodeFreqs y]
  nodeFreqs (Lam t x) =
    mergeFreqs
      [unit (Class "structure") "Lam", typeFreqs t, nodeFreqs x]
    where
      typeFreqs TInt = unit (Class "type") "TInt"
      typeFreqs (Fun t1 t2) =
        mergeFreqs
          [unit (Class "type") "Fun", typeFreqs t1, typeFreqs t2]
  nodeFreqs (Var i) = unit (Class "structure") ("Var_" ++ show i)
  nodeFreqs (App x y) =
    mergeFreqs
      [unit (Class "structure") "App", nodeFreqs x, nodeFreqs y]
  ungenerate (Int i) = [['0' .. '9'] !! i]
  ungenerate (Plus x y) = 'p' : ungenerate x ++ ungenerate y
  ungenerate (Lam t x) =
    'l' : ungenerateTy t ++ ungenerate x
    where
      ungenerateTy TInt = "i"
      ungenerateTy (Fun t1 t2) = 'f' : ungenerateTy t1 ++ ungenerateTy t2
  ungenerate (Var i) = ["x", "y", "z"] !! i
  ungenerate (App x y) = 'a' : ungenerate x ++ ungenerate y

  subtrees n@(Int _) = Set.singleton n
  subtrees n@(Plus x y) = Set.unions [Set.singleton n, subtrees x, subtrees y]
  subtrees n@(Lam _ x) = Set.insert n (subtrees x)
  subtrees n@(Var _) = Set.singleton n
  subtrees n@(App x y) = Set.unions [Set.singleton n, subtrees x, subtrees y]
