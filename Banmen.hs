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
    } deriving (Eq, Read, Show)

newtype PrettyBanmen = PB Banmen

instance Show PrettyBanmen where
    showsPrec _ (PB Banmen{..}) = foldl1 (\a b -> a . showString "\n" . b) komalines
        where
        komalines = teban : joinKoma "sente" _senteMochigoma : joinKoma "koute" _kouteMochigoma : banmenlines
        teban = showString $ if _isSente then "next: v" else "next: ^"
        joinKoma msg komas = showString (msg ++ " mochigoma: ") . shows komas
        joinS = foldl (.) id
        banmenlines = [joinS [showCell $ _banmen ! Pos (suji, dan) | suji <- [9,8..1]] | dan <- [1..9]]
        showCell cell = showString "|" . maybe (showString "   ") showKoma cell
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
    dan2 = Nothing : Just Kaku : replicate 5 Nothing ++ [Just Hi, Nothing]
    dan3 = replicate 9 (Just Fu)
    empty = replicate 3 $ replicate 9 Nothing
    color isSente komas = map (fmap ((,) isSente)) komas


