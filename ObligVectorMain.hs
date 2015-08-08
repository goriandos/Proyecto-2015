import ObligVector
import ObligRandomTime
import System.Environment
import Control.Parallel
import Control.Parallel.Strategies


-- Uso:
	-- ghc -O2 ObligVectorMain.hs -rtsopts -threaded
	-- ./ObligVectorMain opcion seed largo +RTS -N2 -s
	
	-- ghc -O2 ObligVectorMain.hs -threaded -rtsopts -eventlog
	-- ./ObligVectorMain opcion seed largo +RTS -N2 -l

main :: IO ()
main = 
	do {
		parametros <- getArgs
		; let params = parsearInt $ tail parametros
		; let semilla = fst params
		; let largo = snd params
		; case (head parametros) of {
					-- producto de 2 vectores SIN paralelismo
			; "1" -> mainVectorD semilla largo
					-- producto de 2 vectores divididos en dos, usa Eval
			; "2" -> mainVectorD_div2 semilla largo
					-- producto de 2 vectores usando Eval, la division se hace en la funcion llamada
			; "3" -> mainParVectorD semilla largo 
					-- producto de 2 vectores usando Eval, usa una version paralela de zipWith
			; "4" -> mainParZipVectorD semilla largo 
					-- producto de 2 vectores con Strategy, usa la version de zipWith y rseq
			; "5" -> mainVectorStrat semilla largo 
					-- producto de 2 vectores con Par Monad, divide los vectores a la mitad
			; "6" -> mainFork semilla largo 
			; otherwise -> print "Uso: ./ObligVectorMain opcion seed largo +RTS -N2 -s"
		}
	}


