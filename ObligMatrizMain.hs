import ObligMatriz
import ObligRandomTime
import System.Environment
import Control.Parallel
import Control.Parallel.Strategies


-- Uso:
	-- ghc -O2 ObligMatrizMain.hs -rtsopts -threaded
	-- ./ObligMatrizMain opcion seed f c r +RTS -N2 -s
	
	-- ghc -O2 ObligMatrizMain.hs -threaded -rtsopts -eventlog
	-- ./ObligMatrizMain opcion seed f c r +RTS -N2 -l

main :: IO ()
main = 
	do {
		parametros <- getArgs
		; let params1 = parsearInt $ tail parametros
		; let semilla = fst params1
		; let f = snd params1
		; let params2 = parsearInt (drop 3 parametros)
		; let c = fst params2
		; let r = snd params2
		; case (head parametros) of {
				-- producto de 2 matrices SIN paralelismo
			; "1" -> mainMatrizD semilla f c r
				-- producto de 2 matrices con Eval
			; "2" -> mainMatrizDPar semilla f c r	
				-- producto de 2 matrices con Strategy
			; "3" -> mainStratM semilla f c r
				-- producto de 2 matrices con Par Monad
			; "4" -> mainForkM semilla f c r
			; otherwise -> print "Uso: ./ObligMatrizMain opcion seed f c r +RTS -N2 -s"
		}
	}


