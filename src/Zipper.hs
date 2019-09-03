{-# LANGUAGE DeriveTraversable #-}

module Zipper where

data Zipper a = Zipper [a] a [a]
  deriving (Eq, Show, Functor, Foldable, Traversable)

fromList :: [a] -> Maybe (Zipper a)
fromList (h:t) = Just $ Zipper [] h t
fromList []    = Nothing

goLeft, goRight :: Zipper a -> Zipper a
goLeft (Zipper (x:l) f r) = Zipper l x (f:r)
goLeft z = z
goRight (Zipper l f (x:r)) = Zipper (f:l) x r
goRight z = z

getFocus :: Zipper a -> a
getFocus (Zipper _ f _) = f
