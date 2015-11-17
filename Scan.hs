module Scan where

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


-- funcion scan SIN paralelismo
scanNoPar :: (Dato -> Dato -> Dato) -> Dato -> ArrayRS -> ArrayRS
scanNoPar f e l = aux 1 f e l

aux :: Int -> (Dato -> Dato -> Dato) -> Dato -> ArrayRS -> ArrayRS
aux n f e l = if n > length l
					then []
					else escan f e ((Prelude.take n) l) : aux (n + 1) f e l

escan :: (Dato -> Dato -> Dato) -> Dato -> ArrayRS -> Dato
escan _ e [] = e
escan f e l = reduceNoPar f e l

-- otra version de scan SIN paralelismo
scan' :: (Dato -> Dato -> Dato) -> Dato -> Dato -> ArrayRS -> ArrayRS
scan' f _ anterior (x:[]) = (anterior `f` x) : []
scan' f e anterior (x:y:xs) = actual : scan' f e actual (y:xs)
								where actual = anterior `f` x


-- scan paralelo usando Eval
scanPar :: (Dato -> Dato -> Dato) -> Dato -> ArrayRS -> Eval (ArrayRS)
scanPar f e l = auxScanPar 1 f e l

auxScanPar :: Int -> (Dato -> Dato -> Dato) -> Dato -> ArrayRS -> Eval ([Dato])
auxScanPar n f e l = if n > length l
						then rpar []
						else do
							a <- escanPar f e ((Prelude.take n) l) 
							b <- auxScanPar (n + 1) f e l
							return (a : b)

escanPar :: (Dato -> Dato -> Dato) -> Dato -> ArrayRS -> Eval (Dato)
escanPar _ e [] = rpar (e)
escanPar f e l = reducePar f e l

-- version paralela de Scan usando Strategy
scanStrat :: (Dato -> Dato -> Dato) -> Dato -> ArrayRS -> ArrayRS
scanStrat f e arr = auxScanStrat 1 f e arr

auxScanStrat :: Int -> (Dato -> Dato -> Dato) -> Dato -> ArrayRS -> ArrayRS
auxScanStrat n f e arr = if n > length arr - 1
                            then parStratFoldR f e arr : []
                            else parStratFoldR f e arr' : auxScanStrat (n + 1) f e arr
                                  where arr' = Prelude.take n arr

-- version paralela de Scan usando Par Monad
scanParMonad :: (Dato -> Dato -> Dato) -> Dato -> ArrayRS -> ArrayRS
scanParMonad f e vals = runPar ( do
        ; let m = (length vals) `div` 2
        ; let v1 = Prelude.take m vals
        ; let auxVals = Prelude.drop m vals
        ; let v2 = Prelude.take m auxVals
        ; i <- new
        ; j <- new
        ; fork (put i (scanNoPar f e v1))
        ; fork (put j (scanNoPar f e v2))
        ; a <- get i
        ; b <- get j
        ; let c = last a
        ; let m2 = (length b) `div` 2
        ; let b1 = Prelude.take m2 b
        ; let auxB = Prelude.drop m2 b
        ; let b2 = Prelude.take m2 auxB
        ; p <- new
        ; q <- new
        ; fork (put p (Prelude.map (f c) b1))
        ; fork (put q (Prelude.map (f c) b2))
        ; w <- get p
        ; z <- get q
        ; return (a Prelude.++ (w Prelude.++ z)))
		

-- version paralela de Scan usando Repa
-- parametros: funcion, neutro, largo de la lista de datos y lista de datos
scanRepa :: (Dato -> Dato -> Dato) -> Dato -> Int -> [Dato] -> RepaArray D
scanRepa f e largo lista = auxScanRepa arr 0 arr' f e
             where arr = construirArregloRepaD largo lista 
                   arr' = construirArregloRepaD 0 []

auxScanRepa :: RepaArray D -> Int -> RepaArray D -> (Dato -> Dato -> Dato) -> Dato -> RepaArray D
auxScanRepa arr iter arrNuevo f e
   | iter < tam  = res
   | otherwise   = arrNuevo      
               where 
                     (R.Z R.:. tam) = R.extent arr
                     corte = recortar arr 0 iter
                     valor = runIdentity $ R.foldAllP f e corte
                     arrNuevo' = agregarUno arrNuevo valor   
                     res = auxScanRepa arr (iter + 1) arrNuevo' f e

-- version de Scan usando Accelerate
scanAccelerate :: (Exp Dato -> Exp Dato -> Exp Dato) -> Exp Dato -> Int -> [Dato] -> AccArray
scanAccelerate f e n l = run $ A.scanl f e (use arr)
       where arr =  A.fromList (A.Z A.:. n) l :: A.Array A.DIM1 Dato





