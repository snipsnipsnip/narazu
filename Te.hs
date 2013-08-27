{-# LANGUAGE RecordWildCards #-}

module Te where

import Banmen
import Pos
import Data.List
import Data.Array
import Koma
import Control.Arrow (second)

data Te = Te
    { _from, _to :: Pos
    , _nari :: Bool
    } deriving (Show, Eq)

applyTe :: Te -> Banmen -> Banmen
applyTe Te{..} Banmen{..} = Banmen
    { _banmen = _banmen // [(_from, Nothing), (_to, Just $ applyIf _nari (second promote) moving)]
    , _senteMochigoma = applyIf _isSente adjustMochigoma _senteMochigoma
    , _kouteMochigoma = applyIf (not _isSente) adjustMochigoma _kouteMochigoma
    , _isSente = not _isSente
    }
    where

    moving
        | isUsingMochigoma = (_isSente, movingMochigoma)
        | otherwise = let Just k = _banmen ! _from in k
    
    captured = _banmen ! _to
    
    adjustMochigoma = removeMochigoma . mayAddCaptured
        where
        removeMochigoma = applyIf isUsingMochigoma $ const $ mochigomaBefore ++ mochigomaAfter
        
        mayAddCaptured = maybe id ((:) . removeSide) captured

        removeSide (side, koma)
            | side == _isSente = error "applyTe: you're capturing ally!"
            | otherwise = koma

    isUsingMochigoma = dan _from < 1
    (mochigomaBefore, movingMochigoma : mochigomaAfter) = splitAt (suji _from) deck
        where
        deck = if _isSente then _senteMochigoma else _kouteMochigoma
    
    applyIf False _ x = x
    applyIf True f x = f x


