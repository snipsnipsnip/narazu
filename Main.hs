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
import Data.List
import Control.Monad

-- $(makeLenses [''Kyokumen])

type KikiMap = Array Pos Int

listKoma :: Banmen -> [(Pos, (Bool, Koma))]
listKoma Banmen{..} = do
	dan <- [1..9]
	suji <- [1..9]
	let pos = Pos (dan, suji)
	koma <- maybeToList $ _banmen ! pos
	return (pos, koma)

listKikiKoma :: Banmen -> Pos -> [(Pos, (Bool, Koma))]
listKikiKoma banmen pos = filter kiki (listKoma banmen)
	where
	kiki (komapos, (_, koma)) = pos `elem` reachablePosFrom komapos banmen

reachablePosFrom :: Pos -> Banmen -> [Pos]
reachablePosFrom from Banmen{..} = maybe [] filterReachableCells $ _banmen ! from
	where
	filterReachableCells (side, koma) = do
		path <- nirami koma
		unfoldr scanEnemyOrBlock $ map offset path
		where
		offset dir = flipDir dir <> from
		flipDir = if side then id else negatePos

		scanEnemyOrBlock [] = Nothing
		scanEnemyOrBlock (p:rest) = do
			guard $ inBoard p
			case _banmen ! p of
				Just (pside, _) -> do
					guard $ pside /= side
					return (p, [])
				_ -> return (p, rest)

t a b c d = applyTe (Te (Pos (a,b)) (Pos (c,d)) True)

