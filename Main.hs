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
	kiki (komapos, koma) = pos `elem` reachablePosFrom (Just koma) komapos banmen

reachablePosFrom :: Maybe (Bool, Koma) -> Pos -> Banmen -> [Pos]
reachablePosFrom mkoma from Banmen{..} = maybe [] filterReachableCells $ mkoma `mplus` (_banmen ! from)
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
		to <- reachablePosFrom mkoma pos b
		nari <- choosePromote koma pos to b
		return $ Te pos to nari
	
	listHariTe = do
		(i, koma) <- zip [0..] $ if _isSente then _senteMochigoma else _kouteMochigoma
		suji <- [1..9]
		let sujikoma = [(pos, _banmen ! pos) | dan <- [1..9], let pos = Pos (suji, dan)]
		guard $ checkNifu koma sujikoma
		(to, Nothing) <- sujikoma
		guard $ not $ willStuck koma to b
		return $ Te (Pos (i, 0)) to False

	checkNifu koma sujikoma = (koma == Fu &&) $ isNothing $ find (Just (_isSente, Fu) ==) $ map snd sujikoma

willStuck :: Koma -> Pos -> Banmen -> Bool
willStuck koma to b = null $ reachablePosFrom (Just (_isSente b, koma)) to b

choosePromote :: Koma -> Pos -> Pos -> Banmen -> [Bool]
choosePromote koma from to b
	| not (canPromote koma && promoteable) = [False]
	| needPromote = [True]
	| otherwise = [True, False]
    where
    promoteable = any (`elem` promoteArea) [dan from, dan to]
    promoteArea = if _isSente b then [7..9] else [1..3]
    needPromote = willStuck koma to b

main = loop 1 initialBanmen
	where
	loop n banmen = do
		--print banmen
		print n
		print $ PB banmen
		let tes = listTe banmen
		putStrLn $ "hands: " ++ show (length tes)
		te <- fmap (tes !!) $ randomRIO (0, length tes - 1)
		print te
		if inBoard $ _from te
			then print $ _banmen banmen ! _from te
			else print $ (if _isSente banmen then _senteMochigoma banmen else _kouteMochigoma banmen) !! suji (_from te)
		unless (Ou `elem` _senteMochigoma banmen || Ou `elem` _kouteMochigoma banmen) $ do
			loop (n + 1) $ applyTe te banmen
