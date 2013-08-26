module Koma where

import System.Random

data Koma
	= Fu | Kyo | Kei | Gin | Kin | Kaku | Hi | Ou 
	| To | NariKyo | NariKei | NariGin | Ma | Ryu
	deriving (Eq, Ord, Enum, Bounded, Read)

instance Random Koma where
	randomR (a, b) g = let (n, gg) = randomR (fromEnum a, fromEnum b) g in (toEnum n, gg)
	random = randomR (minBound, maxBound)

instance Show Koma where
	show x = case x of
		Fu -> "歩"
		Kyo -> "香"
		Kei -> "桂"
		Gin -> "銀"
		Kin -> "金"
		Kaku -> "角"
		Hi -> "飛"
		Ou -> "玉"
		To -> "と"
		NariKyo -> "杏"
		NariKei -> "圭"
		NariGin -> "全"
		Ma -> "馬"
		Ryu -> "竜"

promote :: Koma -> Koma
promote Fu = To
promote Kyo = NariKyo
promote Kei = NariKei
promote Gin = NariGin
promote Kaku = Ma
promote Hi = Ryu
promote x = x

isPromoted :: Koma -> Bool
isPromoted To = True
isPromoted NariKyo = True
isPromoted NariKei = True
isPromoted NariGin = True
isPromoted Kin = True
isPromoted Ma = True
isPromoted Ryu = True
isPromoted _ = False