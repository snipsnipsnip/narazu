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
    } deriving (Show, Read, Eq)

{-
チェックしてないもの：
    コマの移動範囲
    二歩
    打ち歩詰め
    持ち駒の添字
    成りエリア
    動けない場所に持ち駒
-}
applyTe :: Te -> Banmen -> Banmen
applyTe Te{..} Banmen{..} = Banmen
    { _banmen = _banmen // removeOldKoma [(_to, Just $ tryPromote moving)]
    , _senteMochigoma = applyIf _isSente adjustMochigoma _senteMochigoma
    , _kouteMochigoma = applyIf (not _isSente) adjustMochigoma _kouteMochigoma
    , _isSente = not _isSente
    }
    where

    moving
        | isUsingMochigoma = (_isSente, movingMochigoma)
        | otherwise = let Just k = _banmen ! _from in k
    
    removeOldKoma = applyIf (not isUsingMochigoma) ((_from, Nothing) :)

    captured = _banmen ! _to

    tryPromote = applyIf _nari (second promote)

    adjustMochigoma = mayAddCaptured . removeMochigoma
        where
        removeMochigoma = applyIf isUsingMochigoma $ const $ mochigomaBefore ++ mochigomaAfter
        
        mayAddCaptured = maybe id ((:) . resetKoma) captured

        resetKoma (side, koma)
            | side == _isSente = error "applyTe: you're capturing ally!"
            | isUsingMochigoma = error "You can't capture with mochigoma!"
            | otherwise = demote koma

    isUsingMochigoma = dan _from < 1
    (mochigomaBefore, movingMochigoma : mochigomaAfter) = splitAt (suji _from) deck
        where
        deck = if _isSente then _senteMochigoma else _kouteMochigoma
    
    applyIf False _ x = x
    applyIf True f x = f x
