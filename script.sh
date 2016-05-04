#!/bin/bash

guardar=$1vectorPar$2.txt

echo
echo "En ejecucion..."

for (( i=1; i <= 30; i++ ))
do
	echo "Ejecucion:" $i "con" $1 "elementos" >> $guardar
	./MultiReduceVector 4 3 $1 +RTS -N$2 >> $guardar
	
	echo "***************************************************" >> $guardar
done


# *************************************
#
#		IMPORTANTE: En cada ejecucion cambiar los nombres de los archivos donde se guardan los resultados
#
#		--> valores usados para 'guardar'
#
#			listas (sec. y par.): $1.txt
#			vector secuencial: $1vectorSP.txt
#			vector paralelo: $1vectorPar$2.txt
#			array secuencial: $1arraySP.txt
#			array paralelo: $1arrayPar$2.txt
#
# *************************************


# version paralela: 
# parametros: largo de la lista y cantidad de procesadores
#./MRmain 7 3 $1 +RTS -N$2 >> guardar

# version secuencial: 
# parametros: largo de la lista
#./MRmain 7 3 $1 -N1 >> guardar


# version secuencial con vectores
# parametros: largo de la lista
#./MultiReduceVector 8 3 $1 >> guardar

# version paralela con vectores
# parametros: largo de la lista y cantidad de procesadores
#./MultiReduceVector 8 3 $1 +RTS -N$2 > guardar


# version secuencial con arrays
# parametros: largo del array
#./MultiReduceArray 2 3 $1 >> guardar

# version paralela con arrays
# parametros: largo del array y cantidad de procesadores
#./MultiReduceArray 2 3 $1 +RTS -N$2 >> guardar

