
import Control.Parallel
import Control.Parallel.Strategies
import Utils
import Reduce
import Scan
import MultiReduce 
import MultiScan
import Data.Array.Repa

main :: IO () 
main = 
     do {
         let a = multiScanParMonad (+) 0 labels valores
         ; print a
         --; let b = multiScan (+) 0 labels valores
         --; print b
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
         ; let d = multiScanAccelerate (+) 0 labels valores
         ; print d
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

