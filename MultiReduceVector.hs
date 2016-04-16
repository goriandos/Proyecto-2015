-- module MultiReduceVector where

import System.Environment
import Utils
import Data.Time.Clock
import RandomTime
import Control.Parallel
import Control.Parallel.Strategies
import Data.Vector as V

type VectorX = Vector Dato

-- Uso:
	-- compilacion:
			-- ghc -O2 MultiReduceVector.hs -rtsopts -threaded
	-- ejecucion:
			-- "semilla" "(cantidad max de etiquetas) - (1)" "cantidad de elementos a procesar"
			-- ./MultiReduceVector seed max largo +RTS -N4 -s

-- para usar eventlog:
			-- ghc -O2 MultiReduceVector.hs -threaded -rtsopts -eventlog
			-- ./MultiReduceVector seed max largo +RTS -N4 -l


-- Multi Reduce sin paralelismo
mReduceSPVector :: (Dato -> Dato -> Dato) -> Dato -> [Int] -> Labels -> VectorX -> ArrayRS
mReduceSPVector _ _ [] _ _ = []
mReduceSPVector f e (n:ns) l v = auxMRSPV f e n l v 0 (V.length v) : mReduceSPVector f e ns l v

auxMRSPV :: (Dato -> Dato -> Dato) -> Dato -> Int -> Labels -> VectorX -> Int -> Int -> Dato
auxMRSPV _ e _ [] _ _ _ = e
auxMRSPV f e n (l:ls) v pos tam
	| n == l	= if pos < tam
						then (v ! pos) `f` (auxMRSPV f e n ls v (pos + 1) tam)
				  else e
	| otherwise	= auxMRSPV f e n ls v (pos + 1) tam
	
	
mReduceParVector :: (Dato -> Dato -> Dato) -> Dato -> [Int] -> Labels -> VectorX -> [Dato]
mReduceParVector _ _ [] _ _ = []
mReduceParVector f e (n:ns) l v = runEval $ do
	a <- rpar (auxMRSPV f e n l v 0 (V.length v))
	--b <- rseq (mReduceParVector f e ns l v)
	b <- rpar (mReduceParVector f e ns l v)
	rseq a
	rseq b
	return (a:b)
	
--vec :: VectorX
--vec = V.replicate 10 1

--labs :: Labels
--labs = [1,2,3,1,2,3,1,2,1,3]

--elems :: [Int]
--elems = [1,2,3]

main :: IO()
main = 
	do {
			parametros <- getArgs
			; let pars = parsearEntradas parametros
			; let semilla = Prelude.head pars 
			; let m = Prelude.head (Prelude.tail (pars))
			; let n = Prelude.head (Prelude.tail (Prelude.tail (pars)))
			; let labs = devolverEtiquetas semilla m n
			; let vec = V.replicate n 1
			; let etiqs = [0,1,2,3]
			; t0 <- getCurrentTime
			--; let r = mReduceSPVector (+) 0 etiqs labs vec
			; let r = mReduceParVector (+) 0 etiqs labs vec
			; printTimeSince t0
			; print r
			; printTimeSince t0
}
