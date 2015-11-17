module MultiScan where

import Control.Parallel
import Control.Parallel.Strategies
import Control.Monad.Par.Scheds.Trace
import Data.Time.Clock
import RandomTime
import Data.Array.Repa as R
import Data.Functor.Identity
import Utils
import Scan
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Interpreter


-- MultiScan sin paralelismo
-- arma una lista de Arreglos cada uno con los valores de Scan correspondientes a
-- cada etiqueta
multiScan :: (Dato -> Dato -> Dato) -> Dato -> Labels -> ArrayRS -> [ArrayRS]
multiScan f e l v = Prelude.map (scanNoPar f e) (construir l v)

-- MultiScan usando Eval
multiScanEval :: (Dato -> Dato -> Dato) -> Dato -> Labels -> ArrayRS -> [Eval (ArrayRS)]
multiScanEval f e labs arr = Prelude.map (scanPar f e) arr'
       where arr' = construir labs arr
       
evaluarMS :: [Eval (ArrayRS)] -> [ArrayRS]
evaluarMS [] = []
evaluarMS (x:xs) = runEval x : evaluarMS xs

-- MultiScan usando Strategy
multiScanStrategy :: (Dato -> Dato -> Dato) -> Dato -> Labels -> ArrayRS -> [ArrayRS]
multiScanStrategy f e labs arr = Prelude.map (scanStrat f e) arr'
        where arr' = construir labs arr

-- MultiScan usando Par Monad
multiScanParMonad :: (Dato -> Dato -> Dato) -> Dato -> Labels -> ArrayRS -> [ArrayRS]
multiScanParMonad f e labs arr = Prelude.map (scanParMonad f e) arr'
       where arr' = construir labs arr 

-- MultiScan usando Repa
multiScanRepa :: (Dato -> Dato -> Dato) -> Dato -> Labels -> ArrayRS -> [RepaArray U]
multiScanRepa f e labs arr = multiScanRepaAux(auxRepaMultiScan f e l)
        where lista = construir labs arr
              longits = largos lista 
              l = armarListaRepaD lista longits

auxRepaMultiScan :: (Dato -> Dato -> Dato) -> Dato -> [RepaArray D] -> [RepaArray D]
auxRepaMultiScan _ _ [] = []
auxRepaMultiScan f e (x:xs) = auxScanRepa x 0 arr' f e : auxRepaMultiScan f e xs
        where arr' = construirArregloRepaD 0 []

multiScanRepaAux :: [RepaArray D] -> [RepaArray U]
multiScanRepaAux [] = []
multiScanRepaAux (x:xs) = computeS x : multiScanRepaAux xs


-- MultiScan usando Accelerate: usa el 'scanl' de la biblioteca
multiScanAccelerate :: (Exp Dato -> Exp Dato -> Exp Dato) -> Exp Dato -> Labels -> ArrayRS -> [AccArray]
multiScanAccelerate f e labs arr = auxMultiScanAccelerate f e l
      where lista = construir labs arr
            longits = largos lista 
            l = armarListaAccelerate lista longits

auxMultiScanAccelerate :: (Exp Dato -> Exp Dato -> Exp Dato) -> Exp Dato -> [AccArray] -> [AccArray]
auxMultiScanAccelerate _ _ [] = []
auxMultiScanAccelerate f e (x:xs) = run (A.scanl f e (use x)) : auxMultiScanAccelerate f e xs
