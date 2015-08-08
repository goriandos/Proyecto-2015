module ObligVector where


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
import ObligRandomTime

-- tipo para los vectores densos
type VectorD = [Int]

-- tamanio de un VectorD
largoV :: VectorD -> Int
largoV v = length v

-- primera mitad de un vector
primeraMitad :: VectorD -> VectorD
primeraMitad v = take (largoV v) v

-- segunda mitad de un vector
segundaMitad :: VectorD -> VectorD
segundaMitad v = drop (largoV v) v

-- calcula el producto de dos vectores densos SIN paralelismo
-- pre: vectores del mismo tamanio
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

-- calcula e imprime los vectores y su producto
-- los vectores se generan en forma aleatoria
-- parametros: semilla para el generador y
-- el largo de cada vector
mainVectorD :: Int -> Int -> IO ()
mainVectorD semilla largo = 
	do {
	    t0 <- getCurrentTime
		; let valores1 = generador semilla
		; let v1 = take largo valores1
		; let valores2 = drop largo valores1
		; let v2 = take largo valores2
		; printTimeSince t0
		; print $ prod_VxV v1 v2
		; printTimeSince t0
		}

-- para calcular el producto de 2 vectores aleatorios usando Eval
-- se llama a:		prodPar_VxV
-- muestra tiempos de ejecucion
-- parametros: semilla para el generador y
-- el largo de cada vector
mainParVectorD :: Int -> Int -> IO ()
mainParVectorD semilla largo =
	do {
		t0 <- getCurrentTime
		; let valores1 = generador semilla
		; let v1 = take (largo `div` 2) valores1
		; let r1 = take largo valores1
		; let v2 = drop (largo `div` 2) r1
		; let r2 = drop largo valores1
		; let w1 = take (largo `div` 2) r2
		; let w2 = drop (largo `div` 2) r2
		; printTimeSince t0
		; let r = runEval (prodPar_VxV v1 v2 w1 w2)
		; printTimeSince t0
		; print r
		; printTimeSince t0
		}

mainParZipVectorD :: Int -> Int -> IO ()
mainParZipVectorD semilla largo =
	do {
	    t0 <- getCurrentTime
		; let valores1 = generador semilla
		; let v1 = take largo valores1
		; let valores2 = drop largo valores1
		; let v2 = take largo valores2
		; printTimeSince t0
		; let r = sum $ runEval (parZipWith (*) v1 v2)
		; print r
		; printTimeSince t0
		}

-- calcula e imprime los vectores y su producto
-- los vectores se generan en forma aleatoria
-- parametros: semilla para el generador y
-- el largo de cada vector
mainVectorD_div2 :: Int -> Int -> IO ()
mainVectorD_div2 semilla largo = 
	do {
	    t0 <- getCurrentTime
		; let valores1 = generador semilla
		; let v1 = take largo valores1
		; let valores2 = drop largo valores1
		; let v2 = take largo valores2
		; printTimeSince t0
		--; r <- evaluate $ runEval $ prodPar_VxV_div2 v1 v2
		; let r = runEval $ prodPar_VxV_div2 v1 v2
		; print r
		; printTimeSince t0
		}

-- utilizando Strategy
mainVectorStrat :: Int -> Int -> IO ()
mainVectorStrat semilla largo =
	do {
		t0 <- getCurrentTime
		; let valores1 = generador semilla
		; let v1 = take largo valores1
		; let valores2 = drop largo valores1
		; let v2 = take largo valores2
		; printTimeSince t0
		; let r = prod_VxV_Strat v1 v2
		; print r
		; printTimeSince t0
	}

-- producto de vectores con Monad Par
-- los vectores se generan en forma aleatoria
-- parametros: semilla para el generador y
-- el largo de cada vector
mainFork :: Int -> Int -> IO ()
mainFork semilla largo =
	do {
		t0 <- getCurrentTime
		; let valores1 = generador semilla
		; let v1 = take largo valores1
		; let valores2 = drop largo valores1
		; let v2 = take largo valores2
		; printTimeSince t0
		; print $ runPar ( do
			; i <- new
			; j <- new
			; fork (put i (prod_VxV (primeraMitad v1) (primeraMitad v2)))
			; fork (put j (prod_VxV (segundaMitad v1) (segundaMitad v2)))
			; a <- get i
			; b <- get j
			; return (a + b))
		; printTimeSince t0
	}
	
		
			
