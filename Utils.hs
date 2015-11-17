module Utils where

import Control.Parallel
import Control.Parallel.Strategies
import Control.Monad.Par.Scheds.Trace
import Data.Time.Clock
import RandomTime
import Data.Array.Repa as R
import Data.Functor.Identity
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Interpreter


type Dato = Int
type ArrayRS = [Dato]  
type Labels = [Int]

type RepaArray r = R.Array r R.DIM1 Dato   
type RepaLabel r = R.Array r R.DIM1 Int  

type AccArray = A.Vector (Dato)

data Result = Dato | Eval | IO (Dato) | Scalar (Dato) | ArrayRS | RepaArray D | AccArray 

-- ******************************
-- funciones y datos para testing
-- ******************************
suma :: Dato -> Dato -> Dato
suma a b = a + b

maximo :: Dato -> Dato -> Dato
maximo a b = if a > b
				then a
				else b

minimo :: Dato -> Dato -> Dato
minimo a b = if a > b
				then b
				else a

arr :: ArrayRS
arr = [1,2,3,4,5]

arr2 :: ArrayRS
arr2 = [2, 4, 8, 1, 5, 7, 3, 6]

labels :: Labels
labels = [1,0,2,2,1,0,0,2,1,0,1,2]

valores :: ArrayRS
valores = [5,3,6,0,1,9,9,3,7,1,4,0]

-- 16 unos
unos :: ArrayRS
unos = [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]

labelUnos :: Labels
labelUnos = [1,2,3,4,4,1,2,3,3,1,4,2,4,3,2,1]

ones :: [Dato]
ones = [1,1..]

listaDieciseisUnos :: [Dato]
listaDieciseisUnos = [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]


-- *******************************

-- constructores de Arrays para Repa
construirArregloRepa :: Int -> RepaArray U
construirArregloRepa n = fromListUnboxed (R.Z R.:. n) (Prelude.take n [1,1..])

construirArregloRepaDeLista :: Int -> [Dato] -> RepaArray U
construirArregloRepaDeLista n l = fromListUnboxed (R.Z R.:. n) l

construirArregloRepaD :: Int -> [Dato] -> RepaArray D
construirArregloRepaD n l = R.delay $ fromListUnboxed (R.Z R.:. n) l

construirLabelsRepaDeLista :: Int -> [Int] -> RepaLabel U
construirLabelsRepaDeLista n l = fromListUnboxed (R.Z R.:. n) lista
									where lista = Prelude.take n l

-- para obtener porciones de un Array en Repa
recortar :: R.Array D R.DIM1 Dato -> Int -> Int -> R.Array D R.DIM1 Dato
recortar arr inicio fin = R.extract (R.Z R.:. inicio) (R.Z R.:. fin) arr

-- agrega un Dato a un Array en Repa
agregarUno :: RepaArray D -> Dato -> RepaArray D
agregarUno arr d = construirArregloRepaD (n + 1) l
      where 
            (R.Z R.:. n) = extent arr
            l' = R.toList arr
            l = l' Prelude.++ [d]

-- devuelve una lista de Arreglos, cada uno de sus elementos tiene asociada la misma etiqueta
-- llamado por MultiReduce y por MultiScan usando Repa
construir :: Labels -> ArrayRS -> [ArrayRS]
construir _ [] = []
construir [] _ = []
construir (l:ls) (v:vs) = (armarListaValores l (l:ls) (v:vs)) : (construir (sacarEtiquetas l ls) (sacarValores l ls vs))

armarListaValores :: Int -> Labels -> ArrayRS -> ArrayRS
armarListaValores _ [] _ = []
armarListaValores _ _ [] = []
armarListaValores n (l:ls) (v:vs) 
		| n == l		= v : armarListaValores n ls vs
		| otherwise		= armarListaValores n ls vs
		
sacarEtiquetas :: Int -> Labels -> [Int]
sacarEtiquetas q l = [x | x <- l, x /= q]

sacarValores :: Int -> Labels -> ArrayRS -> ArrayRS
sacarValores _ [] _ = []
sacarValores _ _ [] = []
sacarValores n (l:ls) (v:vs) 
		| n == l		= sacarValores n ls vs
		| otherwise		= v : sacarValores n ls vs


-- devuelve una lista con los largos de cada lista de una lista de Arreglos
-- usado por MultiReduce
largos :: [ArrayRS] -> [Int]
largos arr = Prelude.map length arr

armarListaRepa :: [ArrayRS] -> [Int] -> [RepaArray U] 
armarListaRepa [] _ = []
armarListaRepa _ [] = []
armarListaRepa (a:aes) (l:ls) = R.fromListUnboxed (R.Z R.:. l) a : armarListaRepa aes ls 

armarListaRepaD :: [ArrayRS] -> [Int] -> [RepaArray D] 
armarListaRepaD [] _ = []
armarListaRepaD _ [] = []
armarListaRepaD (a:aes) (l:ls) = R.delay(R.fromListUnboxed (R.Z R.:. l) a) : armarListaRepaD aes ls 


-- version de foldr con Strategy
-- parListEl :: Strategy a -> Strategy a
parStratFoldR :: (b -> b -> b) -> b -> [b] -> b
parStratFoldR f e xs = foldr f e xs `using` parListEl rseq

parListEl :: Strategy a -> Strategy a
parListEl estrat = evaluarListaStrat (rparWith estrat)

evaluarListaStrat :: Strategy a -> Strategy a
evaluarListaStrat estrat x = return x

-- devuelve una lista de Array para Accelerate
armarListaAccelerate :: [ArrayRS] -> [Int] -> [A.Vector (Dato)]
armarListaAccelerate [] _ = []
armarListaAccelerate _ [] = []
armarListaAccelerate (a:aes) (l:ls) = (A.fromList (A.Z A.:. l) a) : armarListaAccelerate aes ls 




