{-# LANGUAGE RecordWildCards #-}
--{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.QuickCheck

--import Data.Lens.Lazy ( (~=), access, (%=), mapLens, (^=), (^.), (^%=), (^%%=), (^$) )
--import Data.Lens.Template ( makeLenses )

import Banmen
import Koma
import Pos

-- $(makeLenses [''Kyokumen])

data Te = Te
	{ _from, _to :: Pos
	, _nari :: Bool
	, _koma :: Koma
	}

isHari :: Te -> Bool
isHari Te {..} = dan _from == -1

applyTe :: Te -> Banmen -> Banmen
applyTe Te {..} Banmen {..} = Banmen {..}
