{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module Util.Range (contains, RangeClass, Range (..)) where

contains :: (RangeClass r, a ~ RElem r) => r -> a -> Bool
contains r x = (left r `lt` x) && (right r `gt` x)
 where lt = if leftOpen r then (<) else (<=)
       gt = if rightOpen r then (>) else (>=)

class (Ord (RElem r)) => RangeClass r where
  type RElem r :: *
  leftOpen :: r -> Bool
  rightOpen :: r -> Bool
  left :: r -> RElem r
  right :: r -> RElem r
  leftOpen _ = False
  rightOpen _ = False

data Range a = Range a a deriving (Show, Eq)

instance (Ord a) => RangeClass (Range a) where
  type RElem (Range a) = a
  left (Range x y) = x
  right (Range x y) = y

