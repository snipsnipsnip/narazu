{-# LANGUAGE RecordWildCards #-}

module Banmen where

import Koma

-- TODO: use ByteArray
data Banmen = Banmen
	{ _banmen :: [[Koma]]
	, _senteMochigoma, _kouteMochigoma :: [Koma]
	, _teban :: Bool
	} deriving (Eq, Read)

instance Show Banmen where
	showsPrec _ Banmen {..} = foldl (\a b -> a . showString "\n" . b) id komalines
		where
		komalines = teban : joinKoma _senteMochigoma : joinKoma _kouteMochigoma : map joinKoma _banmen
		teban = showString $ if _teban then "先手" else "後手"
		joinKoma = foldl (.) id . map shows

