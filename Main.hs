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

listKoma :: Banmen -> [(Pos, (Bool, Koma))]
listKoma Banmen{..} = do
	dan <- [1..9]
	suji <- [1..9]
	let pos = Pos (suji, dan)
	koma <- maybeToList $ _banmen ! pos
	return (pos, koma)

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

listTe :: Banmen -> [Te]
listTe (b@Banmen{..}) = avoidOute $ listMoveTe ++ listHariTe
	where

	avoidOute = filter ((/= Just True) . isOute _isSente . flip applyTe b)

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
		guard $ not $ willStuck koma to b
		return $ Te (Pos (i, 0)) to False

	checkNifu koma sujikoma = isNothing $ find (Just (_isSente, Fu) ==) $ map snd sujikoma

willStuck :: Koma -> Pos -> Banmen -> Bool
willStuck koma to b = suji to == cliff && koma `elem` [Fu, Kei, Kyo]
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
    needPromote = willStuck koma to b

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

tume = Banmen
    { _banmen = listArray (Pos (1, 1), Pos (9, 9)) (repeat Nothing) // ban
    , _senteMochigoma = [minBound..maxBound] \\ [Ou]
    , _kouteMochigoma = [Kin]
    , _isSente = False
    }
    where
    ban = flip execState [] $ do
    	k 1 1 True Kyo
    	k 2 1 True Kei
    	k 1 2 True Fu
    	k 2 2 True Ou
    	k 3 4 True Ryu
    	k 4 1 False Ma
    	k 4 2 False Ma
    	where
    	k x y color koma = modify ((Pos (x, y), Just (color, koma)):)

--tumeLoop :: Banmen -> [[Banmen]]
--problem = runStateT (problem, []) $
tumeLoop :: StateT (Banmen, [Te]) [] ()
tumeLoop = fix $ \loop -> do
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

solveTume :: [(Banmen, [(Te)])]
solveTume = execStateT tumeLoop (tume, [])

isOute :: Bool -> Banmen -> Maybe Bool
isOute ouSide (b@Banmen{..}) = fmap kiki $ listToMaybe ouPos
	where
	ouPos = [pos | (pos, Just (s, Ou)) <- assocs _banmen, s == ouSide]
	kiki pos = not $ null $ listKikiKoma b pos

isTsumi :: Bool -> Banmen -> Bool
isTsumi ouSide banmen = and [Just True == isOute ouSide (applyTe te banmen) | te <- listTe banmen]
