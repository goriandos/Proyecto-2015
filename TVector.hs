module TVector where


import Control.Parallel
import Control.Parallel.Strategies
import Control.DeepSeq
import Data.List
import System.IO
import Data.Char
import Data.Time.Clock
import Control.Exception
import System.Environment
import Control.Monad.Par.Scheds.Trace
import RandomTime

-- tipo para los vectores 
type VectorD = [Int]

-- tamaño de un VectorD
largoV :: VectorD -> Int
largoV v = length v

-- calcula el producto de dos vectores densos SIN paralelismo
-- pre: vectores del mismo tamaño
prod_VxV :: VectorD -> VectorD -> Int
prod_VxV v1 v2 = sum(zipWith (*) v1 v2)

-- calcula el producto vectorial de 2 vectores densos utilizando Eval
-- se calcula el prod vectorial para la primera y para la 
-- segunda mitad de los dos vectores dados y luego se suma los resultados parciales
-- pre: los vectores son del mismo tamanio y ademas
-- ese valor es un numero par
prodPar_VxV_div2 :: VectorD -> VectorD -> Eval Int
prodPar_VxV_div2 v1 v2 = do
	a <- rpar(prod_VxV (take ((largoV v1) `div` 2) v1) (take ((largoV v2) `div` 2) v2))
	--b <- rpar(prod_VxV (drop ((largoV v1) `div` 2) v1) (drop ((largoV v2) `div` 2) v2))
	b <- rseq(prod_VxV (drop ((largoV v1) `div` 2) v1) (drop ((largoV v2) `div` 2) v2))
	rseq a
	--rseq b
	return (a + b)
	
-- calcula el producto vectorial de dos vectores densos utilizando Eval
-- se asume que se pasan como parametros dos vectores partidos por la mitad
-- v = v1 ++ v2 y w = w1 ++ w2
prodPar_VxV :: VectorD -> VectorD -> VectorD -> VectorD -> Eval Int
prodPar_VxV v1 v2 w1 w2 = do
	a <- rpar (prod_VxV v1 w1)
	b <- rpar (prod_VxV v2 w2)
	rseq a 
	rseq b
	return (a + b)

-- version paralela de zipWith
parZipWith :: (a -> b-> c) -> [a] -> [b] -> Eval [c]
parZipWith f [] _ = return []
parZipWith f _ [] = return []
parZipWith f (x:xs) (y:ys) = do
	w <- rpar (f x y)
	ws <- parZipWith f xs ys
	return (w:ws)

-- version de zipWith con Strategy
		-- parList :: Strategy a -> Strategy [a]
parStratZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
parStratZipWith f xs ys = zipWith f xs ys `using` parList rpar

-- producto de 2 vectores usando la version de zipWith con Strategy
prod_VxV_Strat :: VectorD -> VectorD -> Int
prod_VxV_Strat v1 v2 = sum $ parStratZipWith (*) v1 v2

-- version de map con Strategy
stratMap :: (a -> b) -> [a] -> [b]
stratMap f xs = map f xs `using` parList rseq


		
			
