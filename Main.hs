{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

--import Data.Lens.Lazy ( (~=), access, (%=), mapLens, (^=), (^.), (^%=), (^%%=), (^$) )
--import Data.Lens.Template ( makeLenses )

import Banmen
import Koma

data Kyokumen = Kyokumen
	{ _banmen :: Banmen
	, _sengo :: Bool
	}
