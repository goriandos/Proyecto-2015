module ObligMatriz where

import Control.Parallel
import Control.Parallel.Strategies
import Data.List
import Control.Exception
import System.Environment
import Control.Monad.Par.Scheds.Trace
import Data.Time.Clock
import ObligVector
import ObligRandomTime


-- tipo para las matrices densas
-- cant filas: largo de [VectorD]
-- cant columnas: largo de los VectorD
type MatrizD = [VectorD]

-- cantidad de filas de una MatrizD
filas :: MatrizD -> Int
filas m = length m

-- cantidad de columnas de una MatrizD
columnas :: MatrizD -> Int
columnas (m:ms) = length m

-- producto de una matriz por un vector, ambos densos
-- no usa paralelismo
-- pre: la matriz es (m x n) y el vector es (n x 1)
prod_MxV :: MatrizD -> VectorD -> VectorD
prod_MxV m v = map (prod_VxV v) m 

-- producto paralelo de matriz por vector
prodPar_MxV :: MatrizD -> VectorD -> [Eval Int]
prodPar_MxV m v = do 
	map (prodPar_VxV_div2 v) m
	
-- producto de dos matrices densas SIN paralelismo
-- pre: m1 es (m x p) y m2 es (p x n)
-- el producto resultante es (m x n)
prod_MxM :: MatrizD -> MatrizD -> MatrizD
--prod_MxM m1 m2 = transpose $ map (prod_MxV m1) (transpose m2)
prod_MxM m1 m2 = map (prod_MxV m1) m2

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
	
-- 
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

izq :: MatrizD -> MatrizD
izq m = map (cortarPrim) m

der :: MatrizD -> MatrizD
der m = map (cortarSeg) m

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
-- parametros: semilla para el generador
--             cantidad de filas de la primera matriz
--             cantidad de columnas de la primera matriz
--             cantidad de columnas de la segunda matriz
--             (la cantidad de filas de la segunda matriz se toma igual a
--             la cantidad de columnas de la primera matriz)
mainMatrizD :: Int -> Int -> Int -> Int -> IO()
mainMatrizD semilla f c r = 
	do {
		t0 <- getCurrentTime
		; let valores1 = generador semilla
		; let m1 = armarMatrizD f f c valores1
		; let valores2 = drop (f * c) valores1
		; let m2 = armarMatrizD c c r valores2
		; printTimeSince t0
		; print $ filas $ prod_MxM m1 m2
		; printTimeSince t0
		}

-- utiliza el map paralelo
mainMatrizDPar :: Int -> Int -> Int -> Int -> IO()
mainMatrizDPar semilla f c r = 
	do {
		t0 <- getCurrentTime
		; let valores1 = generador semilla
		; let m1 = armarMatrizD f f c valores1
		; let valores2 = drop (f * c) valores1
		; let m2 = armarMatrizD c c r valores2
		; printTimeSince t0
		; let r = filas $ runEval(prodPar_MxM_paraMap m1 m2)
		; print r
		; printTimeSince t0
		}

mainStratM :: Int -> Int -> Int -> Int -> IO ()
mainStratM semilla f c r = 
	do {
		t0 <- getCurrentTime
		; let valores1 = generador semilla
		; let m1 = armarMatrizD f f c valores1
		; let valores2 = drop (f * c) valores1
		; let m2 = armarMatrizD c c r valores2
		; printTimeSince t0
		; print $ filas $ prodStrat_MxM m1 m2
		; printTimeSince t0
	}


mainForkM :: Int -> Int -> Int -> Int -> IO()
mainForkM semilla f c r =
	do {
		t0 <- getCurrentTime
		; let valores1 = generador semilla
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
			; return (filas a + filas b))
		; printTimeSince t0
		}

a :: MatrizD
a = [[1,0,0],
	[0,1,2],
	[1,2,2],
	[1,2,1],
	[2,2,2]]
	
b :: MatrizD
b = [[1,0,1,1],
	[1,1,1,2],
	[1,1,0,1]]
