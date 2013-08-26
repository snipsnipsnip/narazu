module Banmen
( Banmen ()
, senteMochigoma
, kouteMochigoma
, banmen
) where

import Koma

-- TODO: use ByteArray
newtype Banmen = Banmen {unBanmen :: [[Koma]]} deriving (Eq)

instance Show Banmen where
	show = show . unBanmen

senteMochigoma, kouteMochigoma :: Banmen -> [Koma]
senteMochigoma = head . unBanmen
kouteMochigoma = head . tail . unBanmen

banmen :: Banmen -> [[Koma]]
banmen = tail . tail . unBanmen


