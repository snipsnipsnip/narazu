{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Banmen where

import Data.Array
import Data.List
import Koma
import Pos
import Control.Monad.State

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

newtype MBanmen a = MBanmen
    { runMBanmen :: State [(Pos, Maybe (Bool, Koma))] a
    } deriving (Functor, Monad)

addKoma x y color koma = MBanmen $ modify ((Pos (x, y), Just (color, koma)):)

makeTsumeBanmen :: MBanmen [Koma] -> Banmen
makeTsumeBanmen definition = Banmen
    { _banmen = listArray (Pos (1, 1), Pos (9, 9)) (repeat Nothing) // defs
    , _senteMochigoma = gyokukataMochigoma 
    , _kouteMochigoma = semekataMochigoma
    , _isSente = False
    }
    where
    (semekataMochigoma, defs) = flip runState [] $ runMBanmen definition
    gyokukataMochigoma = (komaSet \\ banmenKoma) \\ semekataMochigoma
    banmenKoma = [demote koma | (_, Just (_, koma)) <- defs]
    komaSet = concat $
        replicate 18 [Fu] ++
        replicate 4 [Kyo, Kei, Gin, Kin] ++
        replicate 2 [Kaku, Hi]
