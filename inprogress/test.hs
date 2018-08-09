{-# LANGUAGE MultiParamTypeClasses #-}

module Tree where

import Control.Applicative

data Tree a = Tip a | Branch (Tree a) (Tree a)

data Crumbs = Here | L Crumbs | R Crumbs

find :: Eq a => a -> Tree a -> Maybe Crumbs
find v (Tip x)
  | v == x = Just Here
  | otherwise = Nothing
find v (Branch l r) = (L <$> find v l) <|> (R <$> find v r)

