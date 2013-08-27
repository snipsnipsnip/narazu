module Koma where

import System.Random
import Pos

data Koma
	= Fu | Kyo | Kei | Gin | Kin | Kaku | Hi | Ou 
	| To | NariKyo | NariKei | NariGin | Ma | Ryu
	deriving (Eq, Ord, Enum, Bounded, Read, Show)

instance Random Koma where
	randomR (a, b) g = let (n, gg) = randomR (fromEnum a, fromEnum b) g in (toEnum n, gg)
	random = randomR (minBound, maxBound)

{-
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
-}

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

nirami :: Koma -> [Pos]
nirami Fu = map Pos [(0, 1)]
nirami Kyo = map Pos [(0, n) | n <- [1..8]]
nirami Kei = map Pos [(1, 2), (-1, 2)]
nirami Gin = map Pos [(-1, 1), (0, 1), (1, 1), (-1, -1), (1, -1)]
nirami Kin = map Pos [(-1, 1), (0, 1), (1, 1), (-1, 0), (1, 0), (0, -1)]
nirami Kaku = diagonalNirami [1..8]
nirami Hi = straightNirami [1..8]
nirami Ou = map Pos [(-1, 1), (0, 1), (1, 1), (-1, 0), (1, 0), (-1, -1), (0, -1), (1, -1)]
nirami To = nirami Kin
nirami NariKyo = nirami Kin
nirami NariKei = nirami Kin
nirami NariGin = nirami Kin
nirami Ma = nirami Ou ++ diagonalNirami [2..8]
nirami Ryu = nirami Ou ++ straightNirami [2..8]

diagonalNirami range = map Pos $ concat [[(n, n), (-n, n), (n, -n), (-n, -n)] | n <- range]
straightNirami range = map Pos $ concat [[(n, 0), (-n, 0), (0, n), (0, -n)] | n <- range]

niramiFrom :: Pos -> Koma -> [Pos]
niramiFrom pos koma = map (<> pos) $ nirami koma

