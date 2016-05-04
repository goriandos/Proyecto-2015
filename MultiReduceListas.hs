import System.Environment
import Utils
import Data.Time.Clock
import RandomTime
import Control.Parallel
import Control.Parallel.Strategies

-- Uso:
	-- compilacion:
			-- ghc -O2 MultiReduceListas.hs -rtsopts -threaded
	-- ejecucion:
			-- "semilla" "(cantidad max de etiquetas) - (1)" "cantidad de elementos a procesar"
			-- ./MultiReduceListas seed max largo +RTS -N4 -s

-- para usar eventlog:
			-- ghc -O2 MultiReduceListas.hs -threaded -rtsopts -eventlog
			-- ./MultiReduceListas seed max largo +RTS -N4 -l



-- MultiReduce secuencial para listas
multiReduceSecLista :: (Dato -> Dato -> Dato) -> Dato -> [Int] -> Labels -> ArrayRS -> ArrayRS
multiReduceSecLista _ _ [] _ _ = []
multiReduceSecLista f e (n:ns) l arr = auxMRLista f e n l arr : multiReduceSecLista f e ns l arr

-- MultiReduce paralelo para listas
multiReduceParLista :: (Dato -> Dato -> Dato) -> Dato -> [Int] -> Labels -> ArrayRS -> [Dato]
multiReduceParLista _ _ [] _ _ = []
multiReduceParLista f e (x:xs) etiqs vals = runEval $ do
    a <- rpar(auxMRLista f e x etiqs vals)
    b <- rseq(multiReduceParLista f e xs etiqs vals)
    --b <- rpar(multiReduceParLista f e xs etiqs vals)
    --rseq a
    --rseq b
    return (a:b)
    
-- funcion auxiliar llamada por los dos casos
auxMRLista :: (Dato -> Dato -> Dato) -> Dato -> Int -> Labels -> ArrayRS -> Dato
auxMRLista _ e _ [] _ = e
auxMRLista _ e _ _ [] = e
auxMRLista f e n (l:ls) (x:xs)
   | n == l      = x `f` (auxMRLista f e n ls xs)
   | otherwise   = auxMRLista f e n ls xs



main :: IO()
main = 
	do {
			parametros <- getArgs
			; let pars = parsearEntradas parametros
			; let semilla = head pars 
			; let m = head (tail (pars))
			; let n = head (tail (tail (pars)))
			; let labs = devolverEtiquetas semilla m n
			; let vec = take n ones
			; let etiqs = [0,1,2,3]
			; t0 <- getCurrentTime
			--; let r = multiReduceSecLista (+) 0 etiqs labs vec
			; let r = multiReduceParLista (+) 0 etiqs labs vec
			; printTimeSince t0
			; print r
			; printTimeSince t0
}
