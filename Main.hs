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
import Control.Monad.State

-- $(makeLenses [''Kyokumen])

type KikiMap = Array Pos Int

listKikiKoma :: Banmen -> Pos -> [(Pos, (Bool, Koma))]
listKikiKoma banmen pos = do
	(komapos, koma@(Just info)) <- assocs $ _banmen banmen
	guard $ pos `elem` reachablePosFrom koma komapos banmen
	return (komapos, info)

-- posにコマがあったらどこまで動けるかを調べる。
-- コマにNothingを渡すと現に盤面にあるコマを使う。
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

filterOute :: Banmen -> [Te] -> [Te]
filterOute b = filter ((/= Just True) . isOute (_isSente b) . flip applyTe b)

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
		guard $ koma /= Fu || checkNifu koma sujikoma
		(to, Nothing) <- sujikoma
		guard $ not $ isStuckAt koma to b
		return $ Te (Pos (i, 0)) to False

	checkNifu koma sujikoma = isNothing $ find (Just (_isSente, Fu) ==) $ map snd sujikoma

isStuckAt :: Koma -> Pos -> Banmen -> Bool
isStuckAt koma to b = case koma of
	Fu -> suji to == cliff
	Kyo -> suji to == cliff
	Kei -> suji to `elem` [cliff, cliff + 1, cliff - 1]
	_ -> False
	where
	cliff = if _isSente b then 9 else 1

choosePromote :: Koma -> Pos -> Pos -> Banmen -> [Bool]
choosePromote koma from to b
	| not (canPromote koma && promoteable) = [False]
	| needPromote = [True]
	| otherwise = [True, False]
    where
    promoteable = any (`elem` promoteArea) [dan from, dan to]
    promoteArea = if _isSente b then [7..9] else [1..3]
    needPromote = isStuckAt koma to b

main = tekitouLoop 1 initialBanmen

tekitouLoop n banmen = do
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
		tekitouLoop (n + 1) $ applyTe te banmen

-- http://www.shogitown.com/beginner/2013tume/tume13-08.html
tume = makeTsumeBanmen $ do
	addKoma 1 1 True Kyo
	addKoma 2 1 True Kei
	addKoma 1 2 True Fu
	addKoma 2 2 True Ou
	addKoma 3 4 True Ryu
	addKoma 4 1 False Ma
	addKoma 4 2 False Ma
	return [Kin]

tume2 = makeTsumeBanmen $ do
	addKoma 4 1 False Kaku
	addKoma 4 2 False Kaku
	addKoma 1 1 True Kyo
	addKoma 2 1 True Ou
	return [Kin]

tume3 = makeTsumeBanmen $ do
	addKoma 3 1 False Kaku
	addKoma 4 2 False Kaku
	addKoma 1 1 True Kyo
	addKoma 2 1 True Ou
	return [Kin]

--tumeLoop :: Banmen -> [[Banmen]]
--problem = runStateT (problem, []) $
solveTume :: Banmen -> [(Banmen, [(Te)])]
solveTume problem = flip execStateT (problem, []) $ fix $ \loop -> do
	(banmen, history) <- get

	if length history < 3
		then do
			guard $ isOute True banmen == Just (_isSente banmen)

			te <- lift $ listTe banmen
			let newBanmen = applyTe te banmen
			put (newBanmen, te:history)
			loop
		else do
			guard $ isTsumi True banmen

isOute :: Bool -> Banmen -> Maybe Bool
isOute ouSide (b@Banmen{..}) = fmap kiki $ listToMaybe ouPos
	where
	ouPos = [pos | (pos, Just (s, Ou)) <- assocs _banmen, s == ouSide]
	kiki pos = not $ null $ listKikiKoma b pos

isTsumi :: Bool -> Banmen -> Bool
isTsumi ouSide banmen = and [Just True == isOute ouSide (applyTe te banmen) | te <- listTe banmen]

printSolved solutions = mapM_ (print.map (\t -> (_from t,_to t)) . reverse . snd) solutions