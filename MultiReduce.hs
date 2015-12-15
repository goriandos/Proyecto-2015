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

mRSinPar :: (Dato -> Dato -> Dato) -> Dato -> [Int] -> Labels -> ArrayRS -> ArrayRS
mRSinPar _ _ [] _ _ = []
mRSinPar f e (n:ns) l arr = auxMRUno f e n l arr : mRSinPar f e ns l arr

-- MultiReduce usando Eval
multiReduceEval :: (Dato -> Dato -> Dato) -> Dato -> Labels -> ArrayRS -> [Dato]
multiReduceEval f e labs arr = Prelude.map (reducePar f e) arr'
       where arr' = construir labs arr

multiReduceEval3 :: (Dato -> Dato -> Dato) -> Dato -> Labels -> ArrayRS -> [Dato]
multiReduceEval3 f e labs arr = Prelude.map (reducePar2 f e) arr'
       where arr' = construir labs arr

multiReduceEval' :: (Dato -> Dato -> Dato) -> Dato -> Labels -> ArrayRS -> [Eval (Dato)]
multiReduceEval' f e labs arr = Prelude.map (parallelFoldR f e) arr'
       where arr' = construir labs arr

multiReduceEval2 :: (Dato -> Dato -> Dato) -> Dato -> Labels -> ArrayRS -> [Dato]
multiReduceEval2 f e labs arr = evaluarMR $ Prelude.map (parallelFoldR f e) arr'
       where arr' = construir labs arr

evaluarMR :: [Eval (Dato)] -> [Dato]
evaluarMR [] = []
evaluarMR (x:xs) = runEval x : evaluarMR xs

evalMRLista :: [Eval [Dato]] -> [Dato]
evalMRLista [] = []
evalMRLista (x:xs) = evalDato x Prelude.++ evalMRLista xs

evalDato :: Eval [Dato] -> [Dato]
evalDato x = runEval x

multiReduceMitad :: (Dato -> Dato -> Dato) -> Dato -> Labels -> ArrayRS -> [Eval (Dato)]
multiReduceMitad f e labs arr = auxMRM f e (construir labs arr)

auxMRM :: (Dato -> Dato -> Dato) -> Dato -> [ArrayRS] -> [Eval (Dato)]
auxMRM f e arr = a Prelude.++ b
      where a = Prelude.map (parallelFoldR f e) (primeraMitad arr)
            b = Prelude.map (parallelFoldR f e) (segundaMitad arr)

multiReduceMitad2 :: (Dato -> Dato -> Dato) -> Dato -> Labels -> ArrayRS -> [Eval (Dato)]
multiReduceMitad2 f e labs arr = Prelude.map (aux3 f e) arr'
                   where arr' = (construir labs arr)

aux3 :: (Dato -> Dato -> Dato) -> Dato -> ArrayRS -> Eval (Dato)
aux3 f e x = do
         a <- rpar (reduceNoPar f e (mitadPrimera x))
         b <- rseq (reduceNoPar f e (mitadSegunda x))
         rseq a
         return ((a `f` b))

multiReduce3 :: (Dato -> Dato -> Dato) -> Dato -> Labels -> ArrayRS -> [Dato]
multiReduce3 f e labs arr = Prelude.map (reducePar3 f e) arr'
        where arr' = construir labs arr

multiReduce4 :: (Dato -> Dato -> Dato) -> Dato -> Labels -> ArrayRS -> [Dato]
multiReduce4 f e labs arr = Prelude.map (reducePar4 f e) arr'
        where arr' = construir labs arr
        
-- esta version aplica un map paralelo y procesa cada arreglo por separado
multiReduce5 :: (Dato -> Dato -> Dato) -> Dato -> Labels -> ArrayRS -> [ArrayRS]
multiReduce5 f e labs arr = runEval $ paralMap (auxMR5 f e) arr'
        where arr' = construir labs arr

auxMR5 :: (Dato -> Dato -> Dato) -> Dato -> [Dato] -> [Dato]
auxMR5 f e arr = (foldr f e arr) : []

paralMap :: (ArrayRS -> ArrayRS) -> [ArrayRS] -> Eval [ArrayRS]
paralMap _ [] = return []
paralMap f (x:xs) = do
       b <- rpar (f x)
       bs <- paralMap f xs
       return (b:bs)
       
-- MultiReduce calculando el resultado por arreglo
-- y en cada arreglo lo resuelve a medida que lo va recorriendo
--
-- hay que pasar como parametro una lista con las etiquetas utilizadas
-- 
multiReduceUno :: (Dato -> Dato -> Dato) -> Dato -> [Int] -> Labels -> ArrayRS -> Eval [Dato]
multiReduceUno _ _ [] _ _ = return []
multiReduceUno f e (n:ns) l arr = do
        a <- rpar(auxMRUno f e n l arr)
        b <- multiReduceUno f e ns l arr
        return (a:b)


auxMRUno :: (Dato -> Dato -> Dato) -> Dato -> Int -> Labels -> ArrayRS -> Dato
auxMRUno _ e _ [] _ = e
auxMRUno _ e _ _ [] = e
auxMRUno f e n (l:ls) (x:xs)
   | n == l      = x `f` (auxMRUno f e n ls xs)
   | otherwise   = auxMRUno f e n ls xs

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

mRP :: (Dato -> Dato -> Dato) -> Dato -> [Int] -> Labels -> ArrayRS -> [Dato]
mRP _ _ [] _ _ = []
mRP f e (n:ns) l arr = runPar $ do
     ; i <- new
     ; j <- new
     ; fork (put i (auxMRUno f e n l arr))
     ; fork (put j (mRP f e ns l arr))
     ; a <- get i
     ; b <- get j
     ; return (a:b)

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
