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
import System.Random

-- $(makeLenses [''Kyokumen])

type KikiMap = Array Pos Int

listKoma :: Banmen -> [(Pos, (Bool, Koma))]
listKoma Banmen{..} = do
	dan <- [1..9]
	suji <- [1..9]
	let pos = Pos (suji, dan)
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

listTe :: Banmen -> [Te]
listTe (b@Banmen{..}) = listMoveTe ++ listHariTe
	where

	listMoveTe = do
		pos <- [Pos (suji, dan) | dan <- [1..9], suji <- [1..9]]
		let mkoma = _banmen ! pos
		guard $ isJust mkoma
		let Just (side, koma) = mkoma
		guard $ side == _isSente
		to <- reachablePosFrom pos b
		nari <- if canPromote koma && doesPromote (Te pos to undefined) b then [True, False] else [False]
		return $ Te pos to nari
	
	listHariTe = do
		(i, koma) <- zip [0..] $ if _isSente then _senteMochigoma else _kouteMochigoma
		suji <- [1..9]
		let sujikoma = [(pos, _banmen ! pos) | dan <- [1..9], let pos = Pos (suji, dan)]
		guard $ checkNifu koma sujikoma
		(to, Nothing) <- sujikoma
		return $ Te (Pos (i, 0)) to False

	checkNifu koma sujikoma = (koma == Fu &&) $ isNothing $ find (Just (_isSente, Fu) ==) $ map snd sujikoma

main = loop initialBanmen
	where
	loop banmen = do
		print banmen
		getLine
		let te = listTe banmen
		i <- randomRIO (0, length te - 1)
		loop $ applyTe (te !! i) banmen
