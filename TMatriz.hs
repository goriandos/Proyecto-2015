module TMatriz where

import Control.Parallel
import Control.Parallel.Strategies
import Data.List
import Control.Exception
import System.Environment
import Control.Monad.Par.Scheds.Trace
import Data.Time.Clock
import TVector
import RandomTime


-- tipo para las matrices 
-- cant filas: largo de [VectorD]
-- cant columnas: largo de los VectorD
type MatrizD = [VectorD]

-- cantidad de filas de una MatrizD
filas :: MatrizD -> Int
filas m = length m

-- producto de una matriz por un vector
-- no usa paralelismo
-- pre: la matriz es (m x n) y el vector es (n x 1)
prod_MxV :: MatrizD -> VectorD -> VectorD
prod_MxV m v = map (prod_VxV v) m 

-- producto paralelo de matriz por vector
prodPar_MxV :: MatrizD -> VectorD -> [Eval Int]
prodPar_MxV m v = do 
	map (prodPar_VxV_div2 v) m
	
-- producto de dos matrices SIN paralelismo
-- pre: m1 es (m x p) y m2 es (p x n)
-- el producto resultante es (m x n)
prod_MxM :: MatrizD -> MatrizD -> MatrizD
prod_MxM m1 m2 = transpose $ map (prod_MxV m1) (transpose m2)

-- producto paralelo de 2 matrices
prodPar_MxM :: MatrizD -> MatrizD -> [[Eval Int]]
prodPar_MxM m1 m2 = do
	map (prodPar_MxV m1) (transpose m2)

-- version paralela de map
paraMap :: (a -> b) -> [a] -> Eval [b]
paraMap f [] = return []
paraMap f (x:xs) = do
	y <- rpar (f x)
	ys <- paraMap f xs
	return (y:ys)

-- producto matriz * vector usando map paralelo
prodPar_MxV_paraMap :: MatrizD -> VectorD -> Eval [Int]
prodPar_MxV_paraMap m v = do 
	paraMap (prod_VxV v) m

-- producto matriz * matriz usando map paralelo	
prodPar_MxM_paraMap :: MatrizD -> MatrizD -> Eval [VectorD]
prodPar_MxM_paraMap m1 m2 = do
	paraMap (prod_MxV m1) (transpose m2)
	
prodStrat_MxM :: MatrizD -> MatrizD -> MatrizD
prodStrat_MxM m1 m2 = transpose $ stratMap (prod_MxV m1) (transpose m2)

-- devuelve una MatrizD con la misma cantidad de columnas
-- y la mitad superior de las filas
-- pre: la cantidad de filas es par
supM :: MatrizD -> MatrizD
supM m = take ((filas m) `div` 2) m

-- devuelve una MatrizD con la misma cantidad de columnas
-- y la mitad inferior de las filas
-- pre: la cantidad de filas es par
infM :: MatrizD -> MatrizD
infM m = drop ((filas m) `div` 2) m

cortarPrim :: VectorD -> VectorD
cortarPrim v = take ((largoV v) `div` 2) v

cortarSeg :: VectorD -> VectorD
cortarSeg v = drop ((largoV v) `div` 2) v

unir :: MatrizD -> MatrizD -> MatrizD
unir m1 m2 = m1 ++ m2

sumarV :: VectorD -> Int
sumarV v = sum v

sumarM :: MatrizD -> Int
sumarM m = sum $ map (sumarV) m


-- ************************************************************************************
--					***********************************************
-- 					SOLO VALIDO PARA MATRICES TOMADAS COMO COLUMNAS
--					***********************************************

-- elimina el primer elemento de cada columna de una matriz
podar :: MatrizD -> MatrizD
podar [] = []
podar ([]:x) = x
podar (m:[]) = [tail m]
podar (m:ms) = [tail m] ++ (podar ms)

prod_MVC :: MatrizD -> VectorD -> VectorD
prod_MVC [[], x] _ = []
prod_MVC [] _ = []
prod_MVC ([]:x) _ = []
prod_MVC _ [] = []
prod_MVC (m:ms) (v:vs) = (prod1 (m:ms) (v:vs)) : prod_MVC (podar (m:ms)) (v:vs)

prod1 :: MatrizD -> VectorD -> Int
prod1 [] _ = 0
prod1 _ [] = 0
prod1 (m:ms) (v:vs) = (head m) * v + (prod1 ms vs)

prod_MMC :: MatrizD -> MatrizD -> MatrizD
prod_MMC m1 m2 = map (prod_MVC m1) m2

prod_MMC_par :: MatrizD -> MatrizD -> Eval[VectorD]
prod_MMC_par m1 m2 = paraMap (prod_MVC m1) m2

-- ************************************************************************************


prod_MxM_div2par :: MatrizD -> MatrizD -> Eval MatrizD
prod_MxM_div2par m1 m2 = do
	a <- rpar (prod_MxM (supM m1) m2)
	b <- rpar (prod_MxM (infM m1) m2)
	rseq a
	rseq b
	return (unir a b)

-- genera dos matrices densas aleatorias
-- parametros:  * un valor para la cantidad de iteraciones que
-- 				debe ser igual al numero de filas de la primera matriz
--				porque se itera sobre ese valor, se usa para mantener el estado
-- 				* la cantidad de filas de la primera matriz (debe ser igual al primer parametro)
--				* la cantidad de columnas de la primera matriz
-- 				* la cantidad de columnas de la segunda matriz, la cantidad de filas de esta va a
--    			ser igual a la cantidad de columnas de la primera matriz
--				* una lista infinita de enteros aleatorios 
-- calcula el producto de las matrices, imprime las matrices y su producto
armarMatrizD :: Int -> Int -> Int -> [Int] -> MatrizD
armarMatrizD 0 _ _ _ = []
armarMatrizD i f c l = take c l : armarMatrizD (i - 1) f c (drop c l)

-- calcula el producto de dos matrices aleatorias SIN paralelismo
-- imprime tiempo de ejecucion y cantidad de filas del resultado
-- parametros: cantidad de filas de la primera matriz
--             cantidad de columnas de la primera matriz
--             cantidad de columnas de la segunda matriz
--             (la cantidad de filas de la segunda matriz se toma igual a
--             la cantidad de columnas de la primera matriz)

mainMatrizD :: Int -> Int -> Int -> IO()
mainMatrizD f c r = 
	do {
		t0 <- getCurrentTime
		; let valores1 = [1,1..]
		; let m1 = armarMatrizD f f c valores1
		; let valores2 = drop (f * c) valores1
		; let m2 = armarMatrizD c c r valores2
		; printTimeSince t0
		; print $ (sumarM $ prod_MxM m1 m2) `div` (f * c)
		; printTimeSince t0
		}

-- utiliza el map paralelo
mainMatrizDPar :: Int -> Int -> Int -> IO()
mainMatrizDPar f c r = 
	do {
		t0 <- getCurrentTime
		; let valores1 = [1,1..]
		; let m1 = armarMatrizD f f c valores1
		; let valores2 = drop (f * c) valores1
		; let m2 = armarMatrizD c c r valores2
		; printTimeSince t0
		; let r = sumarM $ (runEval(prodPar_MxM_paraMap m1 m2)) 
		; print $ r `div` (f * c)
		; printTimeSince t0
		}
		
-- utiliza Eval dividiendo a la mitad la primera matriz
mainMatrizD_div2Par :: Int -> Int -> Int -> IO()
mainMatrizD_div2Par f c r = 
	do {
		t0 <- getCurrentTime
		; let valores1 = [1,1..]
		; let m1 = armarMatrizD f f c valores1
		; let valores2 = drop (f * c) valores1
		; let m2 = armarMatrizD c c r valores2
		; printTimeSince t0
		; let r = sumarM $ runEval(prod_MxM_div2par m1 m2)
		; print $ r `div` (f * c)
		; printTimeSince t0
		}

-- utiliza Strategy
mainStratM :: Int -> Int -> Int -> IO ()
mainStratM f c r = 
	do {
		t0 <- getCurrentTime
		; let valores1 = [1,1..]
		; let m1 = armarMatrizD f f c valores1
		; let valores2 = drop (f * c) valores1
		; let m2 = armarMatrizD c c r valores2
		; printTimeSince t0
		; print $ (sumarM $ prodStrat_MxM m1 m2) `div` (f * c)
		; printTimeSince t0
	}

-- utiliza Par Monad
mainForkM :: Int -> Int -> Int -> IO()
mainForkM f c r =
	do {
		t0 <- getCurrentTime
		; let valores1 = [1,1..]
		; let m1 = armarMatrizD f f c valores1
		; let valores2 = drop (f * c) valores1
		; let m2 = armarMatrizD c c r valores2
		; printTimeSince t0
		; print $ runPar ( do
			; i <- new
			; j <- new
			; fork (put i (prod_MxM (supM m1) m2))
			; fork (put j (prod_MxM (infM m1) m2))
			; a <- get i
			; b <- get j
			; return $ (sumarM a + sumarM b) `div` (f * c))
		; printTimeSince t0
		}

-- para el caso de tomar los Vectores como columnas, SIN paralelismo
mainMatrizCol :: Int -> Int -> Int -> IO()
mainMatrizCol f c r = 
	do {
		t0 <- getCurrentTime
		; let valores1 = [1,1..]
		; let m1 = armarMatrizD f f c valores1
		; let valores2 = drop (f * c) valores1
		; let m2 = armarMatrizD c c r valores2
		; printTimeSince t0
		; print $ (sumarM (prod_MMC m1 m2)) `div` (f * c)
		; printTimeSince t0
		}
		
-- para el caso de tomar los Vectores como columnas, utiliza Eval
mainMatrizColPar :: Int -> Int -> Int -> IO()
mainMatrizColPar f c r = 
	do {
		t0 <- getCurrentTime
		; let valores1 = [1,1..]
		; let m1 = armarMatrizD f f c valores1
		; let valores2 = drop (f * c) valores1
		; let m2 = armarMatrizD c c r valores2
		; printTimeSince t0
		; let r = sumarM $ runEval $ prod_MMC_par m1 m2
		; print $ r `div` (f * c)
		; printTimeSince t0
		}

