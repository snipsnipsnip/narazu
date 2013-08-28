
module Pos
( Pos (..)
, dan
, suji
, negatePos
, inBoard
, IntPos
, module Data.Monoid
) where

import Data.Monoid
import Data.Ix
import Data.Int

newtype Pos = Pos {unPos :: (IntPos, IntPos)} deriving (Eq, Ord, Ix)

type IntPos = Int8

instance Monoid Pos where
	mempty = Pos (0, 0)
	mappend (Pos (a,b)) (Pos (c,d)) = Pos (a+c, b+d)

instance Show Pos where
	show = show . unPos

instance Read Pos where
  readsPrec p = map make . readsPrec p
    where
    make (parsed, rest) = (Pos parsed, rest)

dan, suji :: Num a => Pos -> a
dan = fromIntegral . snd . unPos
suji = fromIntegral . fst . unPos
negatePos (Pos (a,b)) = Pos (a, -b)

inBoard (Pos (a, b)) = 1 <= a && a <= 9 && 1 <= b && b <= 9


