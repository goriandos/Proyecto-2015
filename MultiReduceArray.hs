import System.Environment
import Utils
import Data.Time.Clock
import RandomTime
import Control.Parallel
import Control.Parallel.Strategies
import Data.Array as A


type ArrayX = Array Int Dato

-- Uso:
	-- compilacion:
			-- ghc -O2 MultiReduceArray.hs -rtsopts -threaded
	-- ejecucion:
			-- "semilla" "(cantidad max de etiquetas) - (1)" "cantidad de elementos a procesar"
			-- ./MultiReduceArray seed max largo +RTS -N4 -s

-- para usar eventlog:
			-- ghc -O2 MultiReduceArray.hs -threaded -rtsopts -eventlog
			-- ./MultiReduceArray seed max largo +RTS -N4 -l


-- Multi Reduce sin paralelismo
mReduceSPArray :: (Dato -> Dato -> Dato) -> Dato -> [Int] -> Labels -> ArrayX -> ArrayRS
mReduceSPArray _ _ [] _ _ = []
mReduceSPArray f e (n:ns) l a = auxMRSPA f e n l a 1 (snd (A.bounds a)) : mReduceSPArray f e ns l a

auxMRSPA :: (Dato -> Dato -> Dato) -> Dato -> Int -> Labels -> ArrayX -> Int -> Int -> Dato
auxMRSPA _ e _ [] _ _ _ = e
auxMRSPA f e n (l:ls) a pos tam
	| n == l	= if pos < tam
						then (a ! pos) `f` (auxMRSPA f e n ls a (pos + 1) tam)
				  else e
	| otherwise	= auxMRSPA f e n ls a (pos + 1) tam


-- Multi Reduce usando Eval
mReduceParVector :: (Dato -> Dato -> Dato) -> Dato -> [Int] -> Labels -> ArrayX -> [Dato]
mReduceParVector _ _ [] _ _ = []
mReduceParVector f e (n:ns) l arr = runEval $ do
	a <- rpar (auxMRSPA f e n l arr 1 (snd (A.bounds arr)))
	b <- rseq (mReduceParVector f e ns l arr)
	--b <- rpar (mReduceParVector f e ns l arr)
	--rseq a
	--rseq b
	return (a:b)
	


main :: IO()
main = 
	do {
			parametros <- getArgs
			; let pars = parsearEntradas parametros
			; let semilla = Prelude.head pars 
			; let m = Prelude.head (Prelude.tail (pars))
			; let n = Prelude.head (Prelude.tail (Prelude.tail (pars)))
			; let labs = devolverEtiquetas semilla m n
			; let l = Prelude.take n ones
			; let arr = A.listArray (1,n) l
			; let etiqs = [0,1,2,3]
			; t0 <- getCurrentTime
			--; let r = mReduceSPArray (+) 0 etiqs labs arr
			; let r = mReduceParVector (+) 0 etiqs labs arr
			; printTimeSince t0
			; print r
			; printTimeSince t0
}

