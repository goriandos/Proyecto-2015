module ObligRandomTime where

import System.Random
import System.IO
import Control.Exception
import Data.Time.Clock
import Data.Char
import Text.Printf
import System.Environment

-- generador de numeros seudo-aleatorios
-- a partir de una semilla genera una lista infinita de numeros aleatorios
-- para una misma semilla, la lista se mantiene igual
generador :: Int -> [Int]
generador semilla = randomRs(0, 9) . mkStdGen $ semilla

-- para parsear los argumentos de la linea de comandos
-- solo considera los dos primeros valores de la lista
parsearInt :: [String] -> (Int, Int)            
parsearInt entrada = (parsearValor (head entrada), parsearValor (head (tail entrada)))

parsearValor :: String -> Int
parsearValor (x:[]) = ord x - 48
parsearValor (x:xs) = (ord x - 48) * (10 ^ length xs) + parsearValor xs

-- para mostrar el tiempo transcurrido
printTimeSince t0 = do
  t1 <- getCurrentTime
  printf "time: %.2fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)
