import System.Environment
import Control.Parallel
import Control.Parallel.Strategies
import Utils
import Reduce
import Scan
import MultiReduce 
import MultiScan
import Data.Array.Repa
import Data.Time.Clock
import RandomTime

-- Uso:
	-- compilacion:
	-- ghc -O2 MRmain.hs -rtsopts -threaded
	-- ejecucion: "semilla" "(cantidad max de etiquetas) - (1)" "cantidad de elementos a procesar"
	-- ./MRmain seed max largo +RTS -N4 -s
	
	-- para usar eventlog
	-- ghc -O2 MRmain.hs -threaded -rtsopts -eventlog
	-- ./MRmain seed max largo +RTS -N4 -l
		
main :: IO () 
main = 
     do {
           parametros <- getArgs
           ; let pars = parsearEntradas parametros
           ; let semilla = head pars 
           ; let m = head (tail (pars))
           ; let n = head (tail (tail (pars)))
           ; let labs = devolverEtiquetas semilla m n
           ; let vals = take n ones
           --; let etiqs = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
           ; let etiqs = [0,1,2,3]
--; let etiqs = [0,1,2]
--; let acc = [0,0,0]
           ; t0 <- getCurrentTime
           --; let res = multiScan (+) 0 labs vals
--; let res = mSSPar (+) 0 etiqs labels acc valores
           --; let res = mRSinPar (+) 0 etiqs labs vals
           -- ; let listaR = constMRM labs vals
           --; let tmo = length listaR
           --; print tmo
           -- tiempo de construccion
           --; let res = auxRepaMultiReduce (+) 0 listaR
           --; let res = multiReduceUno (+) 0 etiqs labs vals
           --; let res = multiReduceDos (+) 0 etiqs labs vals
           ; let res = multiReduceTres (+) 0 etiqs labs vals
           ; printTimeSince t0
           --; (print . evalDato) res
           ; print res
           -- tiempo total de ejecucion: para conocer tiempo sin construccion
           -- restar del valor anterior
--; print res
           --; let b = multiScan (+) 0 labs vals
           --; printTimeSince t0
           --; print (darLargosListas b)
           ; printTimeSince t0
           --; let res = multiReduceMitad2 (+) 0 labs vals
           --; (print . evalDato) res
           --let a = scanPar (+) 0 listaDieciseisUnos
           --; print a
           -- ; print l
         -- let a = multiScanParMonad (+) 0 labels valores
         -- ; print a
       --; let c = reduceStrat (+) 0 valores
         --; print c
         --; let d = scanStrat (+) 0 listaDieciseisUnos
         --; print d
          --let e = multiReducePar (+) 0 labels valores
          --; print e
         --; let f = multiReduceStrategy (+) 0 labels valores
         --; print f
         --; let g = multiScanStrategy (+) 0 labels valores
         --; print g
         --; let a = reduceAccelerate (+) 0 16 listaDieciseisUnos
         --; print a
         --; let b = scanAccelerate (+) 0 16 listaDieciseisUnos
         --; print b
         --; let c = multiReduceAccelerate (+) 0 labels valores
         --; print c
         -- ; let d = multiScanAccelerate (+) 0 labels valores
         -- ; print d
          --let r = runEval (scanPar (+) 0 valores)
          --; print r
          --; let mps = spm (+) 0 valores
          --; print mps
        --  let r = multiReduce suma 0 labels valores
		--  let s1 = multiScan suma 0 labels valores
        --  ; print s1
		--  ; reduceParMonad suma 0 unos
		--  ; scanParMonad (+) 0 unos
		--  ; rp <- reduceRepa suma 0 16 listaDieciseisUnos
		--  ; print rp
		--  ; scanParMonad (+) 0 unos
		--  let prima = scan' (+) 0 0 valores
		--  ; print prima
		--  let sc = scanP (+) 0 valores
		--  ; print sc
		--  scr <- scanRepa (+) 0 16 unos
		--  ; let res = computeS scr :: Array U DIM1 Dato
		--  ; print scr
		--  let arreglo = construirArregloRepaD 8 arr2
		--  ; let a = computeS $ recortar arreglo 2 4 :: Array U DIM1 Dato
		--  ; print a
		--  ; let arr = construirArregloRepaDeLista 0 []
	    --  ; print $ extent arr
		--  ; let b = computeS $ agregarUno arreglo 2 :: Array U DIM1 Dato
		--  ; print b
		-- ;let s = computeS $ scanRepa (+) 0 16 listaDieciseisUnos :: Array U DIM1 Dato
		-- ; print s
        -- ; let p = repaMultiReduce (+) 0 labels valores 
        --; print p 
        --;let rms = repaMultiScan (+) 0 labels valores
        --; let rms' = multiScanRepa rms
        --; print rms'
        --; let xrms = multiScanRepa . repaMultiScan $ (+) 0 labels valores 
        --; print xrms
        -- ; let mrev = multiReduceEval (+) 0 labels valores
        --; (print . evaluarMR) mrev
        -- ; let msev = multiScanEval (+) 0 labels valores
        -- ; (print . evaluarMS) msev 
        
}

