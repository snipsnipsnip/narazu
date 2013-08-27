
module Pos
( Pos (..)
, dan
, suji
, module Data.Monoid
) where

import Data.Monoid
import Data.Ix

newtype Pos = Pos {unPos :: (Int, Int)} deriving (Eq, Ord, Read, Ix)

instance Bounded Pos where
	minBound = Pos (-8, -8)
	maxBound = Pos (9, 9)

instance Monoid Pos where
	mempty = Pos (0, 0)
	mappend (Pos (a,b)) (Pos (c,d)) = Pos (a+c, b+d)

dan = fst . unPos
suji = snd . unPos
neg (Pos (a,b)) =  (Pos (-a, -b))

instance Show Pos where
	show = show . unPos
