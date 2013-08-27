{-# LANGUAGE RecordWildCards #-}

module Banmen where

import Data.Array
import Koma
import Pos

-- TODO: use ByteArray
data Banmen = Banmen
    { _banmen :: Array Pos (Maybe Koma)
    , _senteMochigoma, _kouteMochigoma :: [Koma]
    , _isSente :: Bool
    } deriving (Eq, Read)

instance Show Banmen where
    showsPrec _ Banmen{..} = foldl (\a b -> a . showString "\n" . b) id komalines
        where
        komalines = teban : joinKoma _senteMochigoma : joinKoma _kouteMochigoma : banmenlines
        teban = showString $ if _isSente then "先手" else "後手"
        joinKoma = foldl (.) id . map shows
        banmenlines = do
            dan <- [1..9]
            suji <- [1..9]
            return $ maybe (showString "  ") shows $ _banmen ! Pos (suji, dan)

initialBanmen = Banmen
    { _banmen = listArray (Pos (1, 1), Pos (9, 9)) list
    , _senteMochigoma = []
    , _kouteMochigoma = []
    , _isSente = True
    }
    where
    list = dan1 ++ dan2 ++ dan3 ++ empty ++ dan3 ++ reverse dan2 ++ dan1
    dan1 = map Just [Kyo, Kei, Gin, Kin, Ou, Kin, Gin, Kei, Kyo]
    dan2 = Nothing : Just Hi : replicate 5 Nothing ++ [Just Kaku, Nothing]
    dan3 = replicate 9 (Just Fu)
    empty = replicate (9 * 3) Nothing


