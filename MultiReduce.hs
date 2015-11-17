module MultiReduce where

import Control.Parallel
import Control.Parallel.Strategies
import Control.Monad.Par.Scheds.Trace
import Data.Time.Clock
import RandomTime
import Data.Array.Repa as R
import Data.Functor.Identity
import Utils
import Reduce
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Interpreter


-- MultiReduce sin paralelismo
multiReduce :: (Dato -> Dato -> Dato) -> Dato -> Labels -> ArrayRS -> ArrayRS
multiReduce f e l v = Prelude.map (reduceNoPar f e) (construir l v)

-- MultiReduce usando Eval
multiReduceEval :: (Dato -> Dato -> Dato) -> Dato -> Labels -> ArrayRS -> [Eval (Dato)]
multiReduceEval f e labs arr = Prelude.map (reducePar f e) arr'
       where arr' = construir labs arr

evaluarMR :: [Eval (Dato)] -> [Dato]
evaluarMR [] = []
evaluarMR (x:xs) = runEval x : evaluarMR xs

-- MultiReduce usando Strategy
multiReduceStrategy :: (Dato -> Dato -> Dato) -> Dato -> Labels -> ArrayRS -> [Dato]
multiReduceStrategy f e labs arr = Prelude.map (reduceStrat f e) arr'
        where arr' = construir labs arr

-- MultiReduce usando Par Monad
multiReducePar :: (Dato -> Dato -> Dato) -> Dato -> Labels -> ArrayRS -> [Dato]
multiReducePar f e labs arr = 
    do {
         let arr' = construir labs arr
         ; Prelude.map (reduceParMonad f e) arr'
    }


-- MultiReduce usando Repa
repaMultiReduce :: (Dato -> Dato -> Dato) -> Dato -> Labels -> ArrayRS -> ArrayRS
repaMultiReduce f e labs arr = auxRepaMultiReduce f e l
			where lista = construir labs arr
			      longits = largos lista 
			      l = armarListaRepa lista longits
			  
auxRepaMultiReduce :: (Dato -> Dato -> Dato) -> Dato -> [RepaArray U] -> [Dato]
auxRepaMultiReduce _ _ [] = []
auxRepaMultiReduce f e (x:xs) = (runIdentity (R.foldAllP f e x)) : (auxRepaMultiReduce f e xs)

-- MultiReduce usando Accelerate
multiReduceAccelerate :: (Exp Dato -> Exp Dato -> Exp Dato) -> Exp Dato -> Labels -> ArrayRS -> [Scalar (Dato)]
multiReduceAccelerate f e labs arr = auxAccMultiReduce f e l
        where lista = construir labs arr
              longits = largos lista 
              l = armarListaAccelerate lista longits

auxAccMultiReduce :: (Exp Dato -> Exp Dato -> Exp Dato) -> Exp Dato -> [A.Vector (Dato)] -> [Scalar (Dato)]
auxAccMultiReduce _ _ [] = []
auxAccMultiReduce f e (x:xs) = run (A.foldAll f e (use x)) : auxAccMultiReduce f e xs
