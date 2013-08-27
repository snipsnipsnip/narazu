
module Pos
( Pos (..)
, dan
, suji
, negatePos
, inBoard
, module Data.Monoid
) where

import Data.Monoid
import Data.Ix

newtype Pos = Pos {unPos :: (Int, Int)} deriving (Eq, Ord, Read, Ix)

instance Monoid Pos where
	mempty = Pos (0, 0)
	mappend (Pos (a,b)) (Pos (c,d)) = Pos (a+c, b+d)

instance Show Pos where
	show = show . unPos

dan = snd . unPos
suji = fst . unPos
negatePos (Pos (a,b)) =  (Pos (-a, -b))

inBoard (Pos (a, b)) = 1 <= a && a <= 9 && 1 <= b && b <= 9


