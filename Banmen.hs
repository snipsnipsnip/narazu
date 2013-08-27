{-# LANGUAGE RecordWildCards #-}

module Banmen where

import Data.Array
import Data.List
import Koma
import Pos

-- TODO: use ByteArray
data Banmen = Banmen
    { _banmen :: Array Pos (Maybe (Bool, Koma))
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
            return $ maybe (showString "  ") showKoma $ _banmen ! Pos (suji, dan)
        showKoma (True, koma) = showString "v" . shows koma
        showKoma (False, koma) = showString "^" . shows koma

initialBanmen = Banmen
    { _banmen = listArray (Pos (1, 1), Pos (9, 9)) $ concat $ transpose list
    , _senteMochigoma = []
    , _kouteMochigoma = []
    , _isSente = True
    }
    where
    list = map (color True) [dan1, dan2, dan3] ++ empty ++ map (color False) [dan3, reverse dan2, dan1]
    dan1 = map Just [Kyo, Kei, Gin, Kin, Ou, Kin, Gin, Kei, Kyo]
    dan2 = Nothing : Just Hi : replicate 5 Nothing ++ [Just Kaku, Nothing]
    dan3 = replicate 9 (Just Fu)
    empty = replicate 3 $ replicate 9 Nothing
    color isSente komas = map (fmap ((,) isSente)) komas


