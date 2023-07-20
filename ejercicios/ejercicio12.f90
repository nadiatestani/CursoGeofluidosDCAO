PROGRAM ejercicio12

! Programa para calcular los números primos entr 1 y 100. Guardar los números primos en un fichero 'NumerosPrimos.dat'
!con una línea para cada número primo

   IMPLICIT NONE

   REAL                          ::numero_inicial
   INTEGER                          ::i, numero_final, number
   CHARACTER(30)                    ::filename

   filename = "NumerosPrimos.dat"

   !first number
   numero_inicial = 2
   numero_final = 100
   OPEN(UNIT=9, FILE=filename, STATUS='new')
   !guardo el primero en un nuevo archivo
   WRITE(9, 20)(numero_inicial) !WRITE/READ([Nunidad], [formato], END=[numero]) [contenido]
   20 FORMAT(10(F5.2,1X)) !no entiendo esto

   !the others: abro el archivo que ya cree
   cargo_numeros: DO i=INT(numero_inicial+1),numero_final
      IF (MOD(i, 2) /= 0) THEN
         OPEN(UNIT=9, FILE=filename, STATUS='old')
         WRITE(9, 20)(REAL(i))
      END IF
   END DO cargo_numeros

CLOSE(UNIT=9)
END PROGRAM 




