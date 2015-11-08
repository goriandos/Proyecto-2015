--module ReduceScan where

import Control.Parallel
import Control.Parallel.Strategies
import Control.Monad.Par.Scheds.Trace
import Data.Time.Clock
import RandomTime
import Data.Array.Repa as R
import Data.Functor.Identity


type Dato = Int
type Arreglo = [Dato]
type Labels = [Int]

type ArregloRepa r = Array r DIM1 Dato
type LabelsRepa r = Array r DIM1 Int


-- funcion reduce SIN paralelismo
-- parametros: una funcion binaria y asociativa
--				elemento neutro de esa funcion
--				un Arreglo de valores
reduceP :: (Dato -> Dato -> Dato) -> Dato -> Arreglo -> Dato
reduceP f e l = foldr f e l

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

arr :: Arreglo
arr = [1,2,3,4,5]

arr2 :: Arreglo
arr2 = [2, 4, 8, 1, 5, 7, 3, 6]

labels :: Labels
labels = [1,0,2,2,1,0,0,2,1,0,1]

valores :: Arreglo
valores = [5,3,6,0,1,9,9,3,7,1,4]

-- 16 unos
unos :: Arreglo
unos = [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]

labelUnos :: Labels
labelUnos = [1,2,3,4,4,1,2,3,3,1,4,2,4,3,2,1]

ones :: [Dato]
ones = [1,1..]

listaDieciseisUnos :: [Dato]
listaDieciseisUnos = [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]


-- *******************************

-- funcion scan SIN paralelismo
scanP :: (Dato -> Dato -> Dato) -> Dato -> Arreglo -> Arreglo
scanP f e l = aux 1 f e l

aux :: Int -> (Dato -> Dato -> Dato) -> Dato -> Arreglo -> Arreglo
aux n f e l = if n > length l
					then []
					else escan f e ((take n) l) : aux (n + 1) f e l
					
escan :: (Dato -> Dato -> Dato) -> Dato -> Arreglo -> Dato
escan _ e [] = e
escan f e l = reduceP f e l

-- otra version de scan SIN paralelismo
scan' :: (Dato -> Dato -> Dato) -> Dato -> Dato -> Arreglo -> Arreglo
scan' f _ anterior (x:[]) = (anterior `f` x) : []
scan' f e anterior (x:y:xs) = actual : scan' f e actual (y:xs)
								where actual = anterior `f` x

-- version paralela de foldr
paraFoldR :: (Dato -> Dato -> Dato) -> Dato -> Arreglo -> Eval (Dato)
paraFoldR _ e [] = rpar (e)
paraFoldR f e (x:xs) = do
	y <- rpar (e `f` x)
	z <- paraFoldR f e xs
	return (y `f` z)

-- version paralela del reduce: utiliza la version paralela de foldr
reducePar :: (Dato -> Dato -> Dato) -> Dato -> Arreglo -> Eval (Dato)
reducePar f e l = paraFoldR f e l

-- version paralela de Reduce usando Par Monad
reduceParMonad :: (Dato -> Dato -> Dato) -> Dato -> Arreglo -> IO()
reduceParMonad f e vals = 
	do {
		t0 <- getCurrentTime
		; let m = (length vals) `div` 2
		; let v1 = take m vals
		; let auxVals = drop m vals
		; let v2 = take m auxVals
		; printTimeSince t0
		; print $ runPar ( do
			; i <- new
			; j <- new
			; fork (put i (reduceP f e v1))
			; fork (put j (reduceP f e v2))
			; a <- get i
			; b <- get j
			; return (a + b))
		; printTimeSince t0
	}

scanParMonad :: (Dato -> Dato -> Dato) -> Dato -> Arreglo -> IO()
scanParMonad f e vals = 
	do {
		t0 <- getCurrentTime
		; let m = (length vals) `div` 2
		; let v1 = take m vals
		; let auxVals = drop m vals
		; let v2 = take m auxVals
		; printTimeSince t0
		; print $ runPar ( do
			; i <- new
			; j <- new
			; fork (put i (scanP f e v1))
			; fork (put j (scanP f e v2))
			; a <- get i
			; b <- get j
			; let c = last a
			; let m2 = (length b) `div` 2
			; let b1 = take m2 b
			; let auxB = drop m2 b
			; let b2 = take m2 auxB
			; p <- new
			; q <- new
			; fork (put p (Prelude.map (f c) b1))
			; fork (put q (Prelude.map (f c) b2))
			; w <- get p
			; z <- get q
			; return (a Prelude.++ (w Prelude.++ z)))
		; printTimeSince t0
	}


-- scan paralelo usando Eval
scanPar :: (Dato -> Dato -> Dato) -> Dato -> Arreglo -> Eval (Arreglo)
scanPar f e l = auxScanPar 1 f e l

auxScanPar :: Int -> (Dato -> Dato -> Dato) -> Dato -> Arreglo -> Eval ([Dato])
auxScanPar n f e l = if n > length l
						then rpar []
						else do
							a <- escanPar f e ((take n) l) 
							b <- auxScanPar (n + 1) f e l
							return (a : b)
					
escanPar :: (Dato -> Dato -> Dato) -> Dato -> Arreglo -> Eval (Dato)
escanPar _ e [] = rpar (e)
escanPar f e l = reducePar f e l


multiReduce :: (Dato -> Dato -> Dato) -> Dato -> Labels -> Arreglo -> Arreglo
multiReduce f e l v = Prelude.map (reduceP f e) (construir l v)

-- devuelve una lista de Arreglos, cada uno de los cuales tiene asociada la misma etiqueta
-- llamado por MultiReduce y por MultiScan
construir :: Labels -> Arreglo -> [Arreglo]
construir _ [] = []
construir [] _ = []
construir (l:ls) (v:vs) = (armarListaValores l (l:ls) (v:vs)) : (construir (sacarEtiquetas l ls) (sacarValores l ls vs))

armarListaValores :: Int -> Labels -> Arreglo -> Arreglo
armarListaValores _ [] _ = []
armarListaValores _ _ [] = []
armarListaValores n (l:ls) (v:vs) 
		| n == l		= v : armarListaValores n ls vs
		| otherwise		= armarListaValores n ls vs
		
sacarEtiquetas :: Int -> Labels -> [Int]
sacarEtiquetas q l = [x | x <- l, x /= q]

sacarValores :: Int -> Labels -> Arreglo -> Arreglo
sacarValores _ [] _ = []
sacarValores _ _ [] = []
sacarValores n (l:ls) (v:vs) 
		| n == l		= sacarValores n ls vs
		| otherwise		= v : sacarValores n ls vs

-- arma una lista de Arreglos cada uno con los valores de Scan correspondientes a
-- cada etiqueta
multiScan :: (Dato -> Dato -> Dato) -> Dato -> Labels -> Arreglo -> [Arreglo]
multiScan f e l v = Prelude.map (scanP f e) (construir l v)

construirArregloRepa :: Int -> ArregloRepa U
construirArregloRepa n = fromListUnboxed (Z:.n) (take n [1,1..])

construirArregloRepaDeLista :: Int -> [Dato] -> ArregloRepa U
construirArregloRepaDeLista n l = fromListUnboxed (Z:.n) l

construirArregloRepaD :: Int -> [Dato] -> ArregloRepa D
construirArregloRepaD n l = R.delay $ fromListUnboxed (Z:.n) l

construirLabelsRepaDeLista :: Int -> [Int] -> LabelsRepa U
construirLabelsRepaDeLista n l = fromListUnboxed (Z:.n) lista
									where lista = take n l

-- reduce usando Repa:
-- parametros: funcion asociativa y su neutro
--             largo de la lista de Datos y la lista de Datos
reduceRepa :: (Dato -> Dato -> Dato) -> Dato -> Int -> [Dato] -> IO (Dato)
reduceRepa f e n l = runIdentity $ res 
			where res = return $ R.foldAllP f e v
				where v = construirArregloRepaDeLista n l

pairwiseAdd :: Array U DIM1 Int -> Array D DIM1 Int
pairwiseAdd arr = traverse arr halfsize indexfun
                    where halfsize (Z:.i) = (Z:.i `div` 2)
                          indexfun ixf (Z:.i) = (ixf (ix1 (2 * i))) + (ixf (ix1 (2 * i + 1)))

pwaValores :: Array U DIM1 Int
-- pwaValores = fromListUnboxed (ix1 10) [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
pwaValores = fromListUnboxed (ix1 10) [1..10]

-- de prueba
recortar :: Array D DIM1 Dato -> Int -> Int -> Array D DIM1 Dato
recortar arr inicio fin = R.extract (Z :. inicio) (Z :. fin) arr

-- parametros: funcion, neutro, largo de la lista de datos y lista de datos
scanRepa :: (Dato -> Dato -> Dato) -> Dato -> Int -> [Dato] -> ArregloRepa D
scanRepa f e largo lista = auxScanRepa arr 0 arr' f e
             where arr = construirArregloRepaD largo lista 
                   arr' = construirArregloRepaD 0 []
                  -- valor = arr ! (Z :. 0)

auxScanRepa :: ArregloRepa D -> Int -> ArregloRepa D -> (Dato -> Dato -> Dato) -> Dato -> ArregloRepa D
auxScanRepa arr iter arrNuevo f e
   | iter < tam  = res
   | otherwise   = arrNuevo      
               where 
                     (Z :. tam) = R.extent arr
                     corte = recortar arr 0 iter
                     valor = runIdentity $ R.foldAllP f e corte
                    -- nuevo = valor `f` (arr ! (Z :. iter))
                     arrNuevo' = agregarUno arrNuevo valor   
                     res = auxScanRepa arr (iter + 1) arrNuevo' f e


agregarUno :: ArregloRepa D -> Dato -> ArregloRepa D
agregarUno arr d = construirArregloRepaD (n + 1) l
      where 
            (Z :. n) = extent arr
            l' = toList arr
            l = l' Prelude.++ [d]
           
      

myFold :: Array D DIM1 Dato -> (Dato -> Dato -> Dato) -> Dato -> IO (Dato)
myFold arr f e = R.foldAllP f e arr
             
myFold2 :: Array D DIM1 Dato -> (Dato -> Dato -> Dato) -> Dato -> Dato
myFold2 arr f e = runIdentity res
    where
       res = (R.foldAllP f e arr)
                   

-- *****************************************************
--scanRepa :: Array U DIM1 Dato -> Int -> (Dato -> Dato -> Dato) -> Dato -> Array U DIM1 Dato
--scanRepa arr n f e = foldP f e trav

--trav :: Array U DIM1 Dato -> Int -> Array D DIM1 Dato
--trav arr n = traverse arr id $ \g (Z:.i) -> g (Z:.i)

--traverse arr id $ \g (Z:.i) -> g (Z:.i) + resParcial
  --          where resParcial = parcial arr i f e
      

--parcial :: Array U DIM1 Int -> Int -> (Int -> Int -> Int) -> Int -> IO (Int)
--parcial arr i f e = foldAllP f e arr'
--			where arr' = traverse arr (const $ Z:.i) id

-- ***************************************************** 

-- da problema el IO (Dato): no puedo sacarlo
--scanRepa :: (Dato -> Dato -> Dato) -> Dato -> Int -> Arreglo -> Array D DIM1 Dato
--scanRepa f e n l = runIdentity $ res
  --           where res = return $ traverse arr id $ \g (Z:.i) -> g (Z:.i) `f` (parcial arr i f e)
    --               arr = construirArregloRepaDeLista n l
			      
--parcial :: ArregloRepa U -> Int -> (Dato -> Dato -> Dato) -> Dato -> Dato
                             
--scanRepa :: (Dato -> Dato -> Dato) -> Dato -> Int -> Arreglo -> Array D DIM1 Dato
--scanRepa f e n l = traverse arr id indices
  --          where arr = construirArregloRepaDeLista n l
    --              indices ixf (Z:.i) = ixf (ix1 i) 
 

-- NO anda y no se porque
-- da problemas con el Z:.n ???????????????????? probe con los dos auxRepa de abajo
--scanRepa :: (Dato -> Dato -> Dato) -> Dato -> Int -> Arreglo -> IO (Array U DIM1 Dato)
--scanRepa f e n l = auxRepa f e n l 
-- $ construirArregloRepaD n l

--auxRepa :: (Dato -> Dato -> Dato) -> Dato -> Int -> ArregloRepa U -> IO (Array U DIM1 Dato)
--auxRepa f e n l = auxItera f e n l
  --        where auxItera f e 0 l = return l
    --            auxItera f e n l = do l' <- foldP f e l 
      --                                auxItera f e (n - 1) l
        --                                 where arr = traverse l (const $ Z:.n) id

--auxRepa :: (Dato -> Dato -> Dato) -> Dato -> Int -> Arreglo -> IO (Array U DIM1 Dato)
--auxRepa f e n l = foldP f e arr'
  --        where arr = fromListUnboxed (ix1 n) l
    --            arr' = traverse arr id (\g (Z:.n) -> g (Z:.n)) 


main :: IO ()
main =  
	do { 
		--let r = runEval (scanPar suma 0 arr)
		--let r = multiReduce suma 0 labels valores
		--; print r
		--; let s = multiScan suma 0 labels valores
		--; print s
		--; reduceParMonad suma 0 unos
		--; scanParMonad (+) 0 unos
		--; rp <- reduceRepa suma 0 16 listaDieciseisUnos
		--; print rp
		--; scanParMonad (+) 0 unos
		--let prima = scan' (+) 0 0 valores
		--; print prima
		--let sc = scanP (+) 0 valores
		--; print sc
		--scr <- scanRepa (+) 0 16 unos
		--; let res = computeS scr :: Array U DIM1 Dato
		--; print scr
		 let arreglo = construirArregloRepaD 8 arr2
		 ; let a = computeS $ recortar arreglo 2 4 :: Array U DIM1 Dato
		 ; print a
		-- ; let arr = construirArregloRepaDeLista 0 []
	--	 ; print $ extent arr
		 ; let b = computeS $ agregarUno arreglo 2 :: Array U DIM1 Dato
		 ; print b
		 ; c <- myFold arreglo (+) 0 :: IO (Dato)
		 ; print c
		 ; let d = myFold2 arreglo (+) 0
		 ; print d
		 ; let s = computeS $ scanRepa (+) 0 16 listaDieciseisUnos :: Array U DIM1 Dato
		 ; print s
}

