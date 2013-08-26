{-# LANGUAGE RecordWildCards #-}
--{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.QuickCheck

--import Data.Lens.Lazy ( (~=), access, (%=), mapLens, (^=), (^.), (^%=), (^%%=), (^$) )
--import Data.Lens.Template ( makeLenses )

import Banmen
import Koma

-- $(makeLenses [''Kyokumen])

newtype Pos = Pos {unPos :: (Int, Int)} deriving (Eq, Ord, Read)
dan = fst . unPos
suji = snd . unPos

instance Show Pos where
	show = show . unPos

data Te = Te
	{ _from, _to :: Pos
	, _nari :: Bool
	, _koma :: Koma
	}

isHari :: Te -> Bool
isHari Te {..} = dan _from == -1

applyTe :: Te -> Banmen -> Banmen
applyTe Te {..} Banmen {..} = Banmen {..}
