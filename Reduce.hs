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
reducePar :: (Dato -> Dato -> Dato) -> Dato -> ArrayRS -> Dato
reducePar f e l = runEval $ parallelFoldR f e l

--reducePar :: (Dato -> Dato -> Dato) -> Dato -> ArrayRS -> Eval (Dato)
--reducePar f e l = parallelFoldR f e l

reducePar2 :: (Dato -> Dato -> Dato) -> Dato -> ArrayRS -> Dato
reducePar2 _ e [] = e
reducePar2 f e (a:[]) = a `f` e
reducePar2 f e (a:b:[]) = a `f` b
reducePar2 f e (a:b:c:[]) = (a `f` b) `f` c
reducePar2 f e (a:b:c:d:arr) = runEval $ do
       x <- rpar (a `f` b)
       y <- rseq (c `f` d)
       rseq x
       return ((x `f` y) `f` (reducePar2 f e arr))
       
reducePar3 :: (Dato -> Dato -> Dato) -> Dato -> ArrayRS -> Dato
reducePar3 f e [] = e
reducePar3 f e (x:[]) = x `f` e
reducePar3 f e (x:y:xs) = runEval $ do
      a <- rpar (reducePar3 f e (mitadPrimera xs))
      b <- rseq (reducePar3 f e (mitadSegunda xs))
      rseq a
      return ((x `f` y) `f` (a `f` b))

reducePar4 :: (Dato -> Dato -> Dato) -> Dato -> ArrayRS -> Dato
reducePar4 f e arr = runEval $ do
      a <- rpar (reduceNoPar f e (mitadPrimera arr))
      b <- rseq (reduceNoPar f e (mitadSegunda arr))
      rseq a 
      return (a `f` b)

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
       


