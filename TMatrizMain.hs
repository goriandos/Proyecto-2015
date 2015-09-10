import TMatriz
import RandomTime
import System.Environment
import Control.Parallel
import Control.Parallel.Strategies


-- Uso:
	-- ghc -O2 TMatrizMain.hs -rtsopts -threaded
	-- ./TMatrizMain opcion f c r +RTS -N2 -s
	-- f: filas de la 1a. matriz
	-- c: columnas de la 1a. matriz y filas de la 2a.
	-- r: columnas de la 2a. matriz
	-- N: numero de procesadores, p.ej. N2 son 2 CPUs
	-- s: muestra estadisticas de la ejecucion
	
	-- alternativa:
	-- ghc -O2 TMatrizMain.hs -threaded -rtsopts -eventlog
	-- ./TMatrizMain opcion f c r +RTS -N2 -l
	-- para obtener informacion grafica sobre la ejecucion de cada hilo
	-- utiliza 'threadscope'

main :: IO ()
main = 
	do {
		parametros <- getArgs
		; let params1 = parsearInt $ parametros
		; let f = snd params1
		; let params2 = parsearInt (drop 2 parametros)
		; let c = fst params2
		; let r = snd params2
		; case (head parametros) of {
				-- producto de 2 matrices SIN paralelismo
			; "1" -> mainMatrizD f c r
				-- producto de 2 matrices con Eval
			; "2" -> mainMatrizDPar f c r	
				-- producto de 2 matrices con Strategy
			; "3" -> mainStratM f c r
				-- producto de 2 matrices con Par Monad
			; "4" -> mainForkM f c r
				-- divide a la mitad la primer matriz, usa Eval
			; "5" -> mainMatrizD_div2Par f c r
				-- utiliza matrices por columnas SIN paralelismo
			; "6" -> mainMatrizCol f c r
				-- matrices por columnas con Par Monad
			; "7" -> mainMatrizColPar f c r
			; otherwise -> print "Uso: ./TMatrizMain opcion f c r +RTS -N2 -s"
		}
	}
