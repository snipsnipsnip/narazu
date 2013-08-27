{-# LANGUAGE RecordWildCards #-}
--{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.QuickCheck

--import Data.Lens.Lazy ( (~=), access, (%=), mapLens, (^=), (^.), (^%=), (^%%=), (^$) )
--import Data.Lens.Template ( makeLenses )

import Banmen
import Koma
import Pos
import Te
import Data.Array
--import Data.Array.ST
import Data.Maybe
import Debug.Trace

-- $(makeLenses [''Kyokumen])

type KikiMap = Array Pos Int
{-
kikiMap :: Banmen -> Pos -> [Pos]
kikiMap Banmen{..} pos = do
	Just (side, koma) <- _banmen ! pos
	guard $ side == _isSente
	p <- niramiFrom pos koma

	_banmen ! p
-}

reachablePosFrom :: Pos -> Banmen -> [Pos]
reachablePosFrom from Banmen{..} = maybe [] filterReachableCells $ _banmen ! from
	where
	filterReachableCells (side, koma) = do
		path <- nirami koma
		takeWhile notBlocked $ map ((<> from) . flipDir) path
		where
		flipDir = if side then id else negatePos
		notBlocked p = trace (show p) $ inBoard p && case _banmen ! p of
			Just (pside, _) -> pside /= side
			_ -> True

