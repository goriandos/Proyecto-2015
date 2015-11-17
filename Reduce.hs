module Reduce where

import Control.Parallel
import Control.Parallel.Strategies
import Control.Monad.Par.Scheds.Trace
import Data.Time.Clock
import RandomTime
import Data.Array.Repa as R
import Data.Functor.Identity
import Utils
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Interpreter


-- funcion reduce SIN paralelismo
-- parametros: una funcion binaria y asociativa
--				elemento neutro de esa funcion
--				un Arreglo de valores
reduceNoPar :: (Dato -> Dato -> Dato) -> Dato -> ArrayRS -> Dato
reduceNoPar f e l = foldr f e l

-- version paralela de foldr
parallelFoldR :: (Dato -> Dato -> Dato) -> Dato -> ArrayRS -> Eval (Dato)
parallelFoldR _ e [] = rpar (e)
parallelFoldR f e (x:xs) = do
	y <- rpar (e `f` x)
	z <- parallelFoldR f e xs
	return (y `f` z)

-- version paralela del reduce: utiliza la version paralela de foldr
reducePar :: (Dato -> Dato -> Dato) -> Dato -> ArrayRS -> Eval (Dato)
reducePar f e l = parallelFoldR f e l

-- version paralela de reduce usando Strategy
-- parStratFoldR es la version paralela de foldr usando esa biblioteca
reduceStrat :: (Dato -> Dato -> Dato) -> Dato -> ArrayRS -> Dato
reduceStrat f e arr = parStratFoldR f e arr


-- version paralela de Reduce usando Par Monad	
reduceParMonad :: (Dato -> Dato -> Dato) -> Dato -> ArrayRS -> Dato
reduceParMonad f e vals = runPar (do
                   ; let m = (length vals) `div` 2
                   ; let v1 = Prelude.take m vals
                   ; let auxVals = Prelude.drop m vals
                   ; let v2 = Prelude.take m auxVals
                   ; i <- new
                   ; j <- new
                   ; fork (put i (reduceNoPar f e v1))
                   ; fork (put j (reduceNoPar f e v2))
                   ; a <- get i
                   ; b <- get j
                   ; return (a + b))
        

-- reduce usando Repa:
-- parametros: funcion asociativa y su neutro
--             largo de la lista de Datos y la lista de Datos
reduceRepa :: (Dato -> Dato -> Dato) -> Dato -> Int -> [Dato] -> IO (Dato)
reduceRepa f e n l = runIdentity $ res 
			where res = return $ R.foldAllP f e v
				where v = construirArregloRepaDeLista n l

-- reduce usando Repa sin paralelismo, usado por MultiReduce paralelo con Repa
reduceRepaNoPar :: (Dato -> Dato -> Dato) -> Dato -> RepaArray U -> Dato
reduceRepaNoPar f e l = R.foldAllS f e l

-- reduce usando Accelerate
reduceAccelerate :: (Exp Dato -> Exp Dato -> Exp Dato) -> Exp Dato -> Int -> [Dato] -> Scalar (Dato)
reduceAccelerate f e n l = run $ A.foldAll f e (use arr)
       where arr = A.fromList (A.Z A.:. n) l :: A.Array A.DIM1 Dato
       


